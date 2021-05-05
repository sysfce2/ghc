{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE GADTs            #-}

{-

This module contains miscellaneous functions related to renaming.

-}

module GHC.Rename.Utils (
        checkDupRdrNames, checkShadowedRdrNames,
        checkDupNames, checkDupAndShadowedNames, dupNamesErr,
        checkTupSize, checkCTupSize,
        addFvRn, mapFvRn, mapMaybeFvRn,
        warnUnusedMatches, warnUnusedTypePatterns,
        warnUnusedTopBinds, warnUnusedLocalBinds,
        warnForallIdentifier,
        checkUnusedRecordWildcard,
        badQualBndrErr, typeAppErr, badFieldConErr,
        wrapGenSpan, genHsVar, genLHsVar, genHsApp, genHsApps, genLHsApp,
        genAppType,
        genLHsLit, genHsIntegralLit, genHsTyLit, genSimpleConPat,
        genVarPat, genWildPat,
        genSimpleFunBind, genFunBind,

        newLocalBndrRn, newLocalBndrsRn,

        bindLocalNames, bindLocalNamesFV,

        addNameClashErrRn, mkNameClashErr,

        checkInferredVars,
        noNestedForallsContextsErr, addNoNestedForallsContextsErr,

        isIrrefutableHsPatRn
)

where


import GHC.Prelude hiding (unzip)

import GHC.Core.Type
import GHC.Hs
import GHC.Types.Name.Reader
import GHC.Tc.Errors.Types
-- import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Core.DataCon
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.SourceFile
import GHC.Types.SourceText ( SourceText(..), IntegralLit )
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Types.Basic  ( TopLevelFlag(..), Origin(Generated), TypeOrKind )
import GHC.Data.List.SetOps ( removeDupsOn )
import GHC.Data.Maybe ( whenIsJust )
import GHC.Driver.DynFlags
import GHC.Data.FastString
import Control.Monad
import GHC.Settings.Constants ( mAX_TUPLE_SIZE, mAX_CTUPLE_SIZE )
import qualified GHC.LanguageExtensions as LangExt

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE


{-
*********************************************************
*                                                      *
\subsection{Binding}
*                                                      *
*********************************************************
-}

newLocalBndrRn :: LocatedN RdrName -> RnM Name
-- Used for non-top-level binders.  These should
-- never be qualified.
newLocalBndrRn (L loc rdr_name)
  | Just name <- isExact_maybe rdr_name
  = return name -- This happens in code generated by Template Haskell
                -- See Note [Binders in Template Haskell] in "GHC.ThToHs"
  | otherwise
  = do { unless (isUnqual rdr_name)
                (addErrAt (locA loc) (badQualBndrErr rdr_name))
       ; uniq <- newUnique
       ; return (mkInternalName uniq (rdrNameOcc rdr_name) (locA loc)) }

newLocalBndrsRn :: [LocatedN RdrName] -> RnM [Name]
newLocalBndrsRn = mapM newLocalBndrRn

bindLocalNames :: [Name] -> RnM a -> RnM a
bindLocalNames names
  = updLclCtxt $ \ lcl_env ->
    let th_level  = thLevel (tcl_th_ctxt lcl_env)
        th_bndrs' = extendNameEnvList (tcl_th_bndrs lcl_env)
                    [ (n, (NotTopLevel, th_level)) | n <- names ]
        rdr_env'  = extendLocalRdrEnvList (tcl_rdr lcl_env) names
    in lcl_env { tcl_th_bndrs = th_bndrs'
               , tcl_rdr      = rdr_env' }

bindLocalNamesFV :: [Name] -> RnM (a, FreeVars) -> RnM (a, FreeVars)
bindLocalNamesFV names enclosed_scope
  = do  { (result, fvs) <- bindLocalNames names enclosed_scope
        ; return (result, delFVs names fvs) }

-------------------------------------
checkDupRdrNames :: [LocatedN RdrName] -> RnM ()
-- Check for duplicated names in a binding group
checkDupRdrNames rdr_names_w_loc
  = mapM_ (\ ns -> dupNamesErr (getLocA <$> ns) (unLoc <$> ns)) dups
  where
    (_, dups) = removeDupsOn unLoc rdr_names_w_loc

checkDupNames :: [Name] -> RnM ()
-- Check for duplicated names in a binding group
checkDupNames names = check_dup_names (filterOut isSystemName names)
                -- See Note [Binders in Template Haskell] in "GHC.ThToHs"

check_dup_names :: [Name] -> RnM ()
check_dup_names names
  = mapM_ (\ ns -> dupNamesErr (nameSrcSpan <$> ns) (getRdrName <$> ns)) dups
  where
    (_, dups) = removeDupsOn nameOccName names

---------------------
checkShadowedRdrNames :: [LocatedN RdrName] -> RnM ()
checkShadowedRdrNames loc_rdr_names
  = do { envs <- getRdrEnvs
       ; checkShadowedOccs envs get_loc_occ filtered_rdrs }
  where
    filtered_rdrs = filterOut (isExact . unLoc) loc_rdr_names
                -- See Note [Binders in Template Haskell] in "GHC.ThToHs"
    get_loc_occ (L loc rdr) = (locA loc,rdrNameOcc rdr)

checkDupAndShadowedNames :: (GlobalRdrEnv, LocalRdrEnv) -> [Name] -> RnM ()
checkDupAndShadowedNames envs names
  = do { check_dup_names filtered_names
       ; checkShadowedOccs envs get_loc_occ filtered_names }
  where
    filtered_names = filterOut isSystemName names
                -- See Note [Binders in Template Haskell] in "GHC.ThToHs"
    get_loc_occ name = (nameSrcSpan name, nameOccName name)

-------------------------------------
checkShadowedOccs :: (GlobalRdrEnv, LocalRdrEnv)
                  -> (a -> (SrcSpan, OccName))
                  -> [a] -> RnM ()
checkShadowedOccs (global_env,local_env) get_loc_occ ns
  = whenWOptM Opt_WarnNameShadowing $
    do  { traceRn "checkShadowedOccs:shadow" (ppr (map get_loc_occ ns))
        ; mapM_ check_shadow ns }
  where
    check_shadow n
        | startsWithUnderscore occ = return ()  -- Do not report shadowing for "_x"
                                                -- See #3262
        | Just n <- mb_local = complain (ShadowedNameProvenanceLocal (nameSrcLoc n))
        | otherwise = do { gres' <- filterM is_shadowed_gre gres
                         ; when (not . null $ gres') $ complain (ShadowedNameProvenanceGlobal gres') }
        where
          (loc,occ) = get_loc_occ n
          mb_local  = lookupLocalRdrOcc local_env occ
          gres      = lookupGRE_RdrName (AllNameSpaces WantBoth) global_env (mkRdrUnqual occ)
                -- Make an Unqualified RdrName and look that up, so that
                -- we don't find any GREs that are in scope qualified-only

          complain provenance = addDiagnosticAt loc (TcRnShadowedName occ provenance)

    is_shadowed_gre :: GlobalRdrElt -> RnM Bool
        -- Returns False for record selectors that are shadowed, when
        -- punning or wild-cards are on (cf #2723)
    is_shadowed_gre gre | isRecFldGRE gre
        = do { dflags <- getDynFlags
             ; return $ not (xopt LangExt.NamedFieldPuns dflags
                             || xopt LangExt.RecordWildCards dflags) }
    is_shadowed_gre _other = return True

-------------------------------------
-- | Throw an error message if a user attempts to quantify an inferred type
-- variable in a place where specificity cannot be observed. For example,
-- @forall {a}. [a] -> [a]@ would be rejected to the inferred type variable
-- @{a}@, but @forall a. [a] -> [a]@ would be accepted.
-- See @Note [Unobservably inferred type variables]@.
checkInferredVars :: HsDocContext
                  -> LHsSigType GhcPs
                  -> RnM ()
checkInferredVars ctxt ty =
  let bndrs = sig_ty_bndrs ty
  in case filter ((==) InferredSpec . hsTyVarBndrFlag) bndrs of
    [] -> return ()
    iv : ivs -> addErr $
      TcRnWithHsDocContext ctxt $
      TcRnIllegalInferredTyVars (iv NE.:| ivs)
  where
    sig_ty_bndrs :: LHsSigType GhcPs -> [HsTyVarBndr Specificity GhcPs]
    sig_ty_bndrs (L _ (HsSig{sig_bndrs = outer_bndrs}))
      = map unLoc (hsOuterExplicitBndrs outer_bndrs)

{-
Note [Unobservably inferred type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While GHC's parser allows the use of inferred type variables
(e.g., `forall {a}. <...>`) just about anywhere that type variable binders can
appear, there are some situations where the distinction between inferred and
specified type variables cannot be observed. For example, consider this
instance declaration:

  instance forall {a}. Eq (T a) where ...

Making {a} inferred is pointless, as there is no way for user code to
"apply" an instance declaration in a way where the inferred/specified
distinction would make a difference. (Notably, there is no opportunity
for visible type application of an instance declaration.) Anyone who
writes such code is likely confused, so in an attempt to be helpful,
we emit an error message if a user writes code like this. The
checkInferredVars function is responsible for implementing this
restriction.

It turns out to be somewhat cumbersome to enforce this restriction in
certain cases.  Specifically:

* Quantified constraints. In the type `f :: (forall {a}. C a) => Proxy Int`,
  there is no way to observe that {a} is inferred. Nevertheless, actually
  rejecting this code would be tricky, as we would need to reject
  `forall {a}. <...>` as a constraint but *accept* other uses of
  `forall {a}. <...>` as a type (e.g., `g :: (forall {a}. a -> a) -> b -> b`).
  This is quite tedious to do in practice, so we don't bother.

* Default method type signatures (#18432). These are tricky because inferred
  type variables can appear nested, e.g.,

    class C a where
      m         :: forall b. a -> b -> forall c.   c -> c
      default m :: forall b. a -> b -> forall {c}. c -> c
      m _ _ = id

  Robustly checking for nested, inferred type variables ends up being a pain,
  so we don't try to do this.

For now, we simply allow inferred quantifiers to be specified here,
even though doing so is pointless. All we lose is a warning.

Aside from the places where we already use checkInferredVars, most of
the other places where inferred vars don't make sense are in any case
already prohibited from having foralls /at all/.  For example:

  instance forall a. forall {b}. Eq (Either a b) where ...

Here the nested `forall {b}` is already prohibited. (See
Note [No nested foralls or contexts in instance types] in GHC.Hs.Type).
-}

-- | Examines a non-outermost type for @forall@s or contexts, which are assumed
-- to be nested. For example, in the following declaration:
--
-- @
-- instance forall a. forall b. C (Either a b)
-- @
--
-- The outermost @forall a@ is fine, but the nested @forall b@ is not. We
-- invoke 'noNestedForallsContextsErr' on the type @forall b. C (Either a b)@
-- to catch the nested @forall@ and create a suitable error message.
-- 'noNestedForallsContextsErr' returns @'Just' err_msg@ if such a @forall@ or
-- context is found, and returns @Nothing@ otherwise.
--
-- This is currently used in the following places:
--
-- * In GADT constructor types (in 'rnConDecl').
--   See @Note [GADT abstract syntax] (Wrinkle: No nested foralls or contexts)@
--   in "Language.Haskell.Syntax.Decls".
--
-- * In instance declaration types (in 'rnClsIntDecl' and 'rnSrcDerivDecl' in
--   "GHC.Rename.Module" and 'renameSig' in "GHC.Rename.Bind").
--   See @Note [No nested foralls or contexts in instance types]@ in
--   "GHC.Hs.Type".
noNestedForallsContextsErr :: NestedForallsContextsIn
                           -> LHsType GhcRn
                           -> Maybe (SrcSpan, TcRnMessage)
noNestedForallsContextsErr what lty =
  case ignoreParens lty of
    L l (HsForAllTy { hst_tele = tele })
      |  HsForAllVis{} <- tele
         -- The only two places where this function is called correspond to
         -- types of terms, so we give a slightly more descriptive error
         -- message in the event that they contain visible dependent
         -- quantification (currently only allowed in kinds).
      -> Just (locA l, TcRnVDQInTermType Nothing)
      |  HsForAllInvis{} <- tele
      -> Just (locA l, nested_foralls_contexts_err)
    L l (HsQualTy {})
      -> Just (locA l, nested_foralls_contexts_err)
    _ -> Nothing
  where
    nested_foralls_contexts_err =
      TcRnNestedForallsContexts what

-- | A common way to invoke 'noNestedForallsContextsErr'.
addNoNestedForallsContextsErr :: HsDocContext
                              -> NestedForallsContextsIn
                              -> LHsType GhcRn
                              -> RnM ()
addNoNestedForallsContextsErr ctxt what lty =
  whenIsJust (noNestedForallsContextsErr what lty) $ \(l, err_msg) ->
    addErrAt l $ TcRnWithHsDocContext ctxt err_msg

{-
************************************************************************
*                                                                      *
\subsection{Free variable manipulation}
*                                                                      *
************************************************************************
-}

-- A useful utility
addFvRn :: FreeVars -> RnM (thing, FreeVars) -> RnM (thing, FreeVars)
addFvRn fvs1 thing_inside = do { (res, fvs2) <- thing_inside
                               ; return (res, fvs1 `plusFV` fvs2) }

mapFvRn :: Traversable f => (a -> RnM (b, FreeVars)) -> f a -> RnM (f b, FreeVars)
mapFvRn f xs = do
    stuff <- mapM f xs
    case unzip stuff of
        (ys, fvs_s) -> return (ys, foldl' (flip plusFV) emptyFVs fvs_s)
{-# SPECIALIZE mapFvRn :: (a -> RnM (b, FreeVars)) -> [a] -> RnM ([b], FreeVars) #-}

unzip :: Functor f => f (a, b) -> (f a, f b)
unzip = \ xs -> (fmap fst xs, fmap snd xs)
{-# NOINLINE [1] unzip #-}
{-# RULES "unzip/List" unzip = List.unzip #-}

mapMaybeFvRn :: (a -> RnM (b, FreeVars)) -> Maybe a -> RnM (Maybe b, FreeVars)
mapMaybeFvRn _ Nothing = return (Nothing, emptyFVs)
mapMaybeFvRn f (Just x) = do { (y, fvs) <- f x; return (Just y, fvs) }

{-
************************************************************************
*                                                                      *
\subsection{Envt utility functions}
*                                                                      *
************************************************************************
-}

warnUnusedTopBinds :: [GlobalRdrElt] -> RnM ()
warnUnusedTopBinds gres
    = whenWOptM Opt_WarnUnusedTopBinds
    $ do env <- getGblEnv
         let isBoot = isHsBootFile $ tcg_src env
         let noParent gre = case gre_par gre of
                            NoParent -> True
                            _        -> False
             -- Don't warn about unused bindings with parents in
             -- .hs-boot files, as you are sometimes required to give
             -- unused bindings (trac #3449).
             -- HOWEVER, in a signature file, you are never obligated to put a
             -- definition in the main text.  Thus, if you define something
             -- and forget to export it, we really DO want to warn.
             gres' = if isBoot then filter noParent gres
                               else                 gres
         warnUnusedGREs gres'


-- | Checks to see if we need to warn for -Wunused-record-wildcards or
-- -Wredundant-record-wildcards
checkUnusedRecordWildcard :: SrcSpan
                          -> FreeVars
                          -> Maybe [Name]
                          -> RnM ()
checkUnusedRecordWildcard _ _ Nothing     = return ()
checkUnusedRecordWildcard loc _ (Just []) =
  -- Add a new warning if the .. pattern binds no variables
  setSrcSpan loc $ warnRedundantRecordWildcard
checkUnusedRecordWildcard loc fvs (Just dotdot_names) =
  setSrcSpan loc $ warnUnusedRecordWildcard dotdot_names fvs


-- | Produce a warning when the `..` pattern binds no new
-- variables.
--
-- @
--   data P = P { x :: Int }
--
--   foo (P{x, ..}) = x
-- @
--
-- The `..` here doesn't bind any variables as `x` is already bound.
warnRedundantRecordWildcard :: RnM ()
warnRedundantRecordWildcard = addDiagnostic TcRnRedundantRecordWildcard


-- | Produce a warning when no variables bound by a `..` pattern are used.
--
-- @
--   data P = P { x :: Int }
--
--   foo (P{..}) = ()
-- @
--
-- The `..` pattern binds `x` but it is not used in the RHS so we issue
-- a warning.
warnUnusedRecordWildcard :: [Name] -> FreeVars -> RnM ()
warnUnusedRecordWildcard ns used_names = do
  let used = filter (`elemNameSet` used_names) ns
  traceRn "warnUnused" (ppr ns $$ ppr used_names $$ ppr used)
  warnIf (null used) (TcRnUnusedRecordWildcard ns)



warnUnusedLocalBinds, warnUnusedMatches, warnUnusedTypePatterns
  :: [Name] -> FreeVars -> RnM ()
warnUnusedLocalBinds   = check_unused UnusedNameLocalBind
warnUnusedMatches      = check_unused UnusedNameMatch
warnUnusedTypePatterns = check_unused UnusedNameTypePattern

check_unused :: UnusedNameProv -> [Name] -> FreeVars -> RnM ()
check_unused prov bound_names used_names
  = warnUnused prov (filterOut (`elemNameSet` used_names) bound_names)

warnForallIdentifier :: LocatedN RdrName -> RnM ()
warnForallIdentifier (L l rdr_name@(Unqual occ))
  | isKw (fsLit "forall") || isKw (fsLit "∀")
  = addDiagnosticAt (locA l) (TcRnForallIdentifier rdr_name)
  where isKw = (occNameFS occ ==)
warnForallIdentifier _ = return ()

-------------------------
--      Helpers
warnUnusedGREs :: [GlobalRdrElt] -> RnM ()
warnUnusedGREs gres = mapM_ warnUnusedGRE gres

-- NB the Names must not be the names of record fields!
warnUnused :: UnusedNameProv -> [Name] -> RnM ()
warnUnused prov names =
  mapM_ (\ nm -> warnUnused1 prov nm (nameOccName nm)) names

warnUnused1 :: UnusedNameProv -> Name -> OccName -> RnM ()
warnUnused1 prov child child_occ
  = when (reportable child child_occ) $
    warn_unused_name prov (nameSrcSpan child) child_occ

warn_unused_name :: UnusedNameProv -> SrcSpan -> OccName -> RnM ()
warn_unused_name prov span child_occ =
  addDiagnosticAt span (TcRnUnusedName child_occ prov)

warnUnusedGRE :: GlobalRdrElt -> RnM ()
warnUnusedGRE gre@(GRE { gre_lcl = lcl, gre_imp = is })
  | lcl       = warnUnused1 UnusedNameTopDecl nm occ
  | otherwise = when (reportable nm occ) (mapM_ warn is)
  where
    occ = greOccName gre
    nm = greName gre
    warn spec =
      warn_unused_name (UnusedNameImported (importSpecModule spec)) span occ
      where
        span = importSpecLoc spec

-- | Should we report the fact that this 'Name' is unused? The
-- 'OccName' may differ from 'nameOccName' due to
-- DuplicateRecordFields.
reportable :: Name -> OccName -> Bool
reportable child child_occ
  | isWiredInName child
  = False    -- Don't report unused wired-in names
             -- Otherwise we get a zillion warnings
             -- from Data.Tuple
  | otherwise
  = not (startsWithUnderscore child_occ)

{-
Note [Skipping ambiguity errors at use sites of local declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, we do not report ambiguous occurrences at use sites where all the
clashing names are defined locally, because the error will have been reported at
the definition site, and we want to avoid an error cascade.

However, when DuplicateRecordFields is enabled, it is possible to define the
same field name multiple times, so we *do* need to report an error at the use
site when there is ambiguity between multiple fields. Moreover, when
NoFieldSelectors is enabled, it is possible to define a field with the same name
as a non-field, so again we need to report ambiguity at the use site.

We can skip reporting an ambiguity error whenever defining the GREs must have
yielded a duplicate declarations error.  More precisely, we can skip if:

 * there are at least two non-fields amongst the GREs; or

 * there are at least two fields amongst the GREs, and DuplicateRecordFields is
   *disabled*; or

 * there is at least one non-field, at least one field, and NoFieldSelectors is
   *disabled*.

These conditions ensure that a duplicate local declaration will have been
reported.  See also Note [Reporting duplicate local declarations] in
GHC.Rename.Names).

-}

addNameClashErrRn :: RdrName -> NE.NonEmpty GlobalRdrElt -> RnM ()
addNameClashErrRn rdr_name gres
  | all isLocalGRE gres && can_skip
  -- If there are two or more *local* defns, we'll usually have reported that
  -- already, and we don't want an error cascade.
  = return ()
  | otherwise
  = do { gre_env <- getGlobalRdrEnv
       ; addErr $ mkNameClashErr gre_env rdr_name gres }
  where
    -- If all the GREs are defined locally, can we skip reporting an ambiguity
    -- error at use sites, because it will have been reported already? See
    -- Note [Skipping ambiguity errors at use sites of local declarations]
    can_skip = num_non_flds >= 2
            || (num_flds >= 2 && not (isDuplicateRecFldGRE (head flds)))
            || (num_non_flds >= 1 && num_flds >= 1
                                  && not (isNoFieldSelectorGRE (head flds)))
    (flds, non_flds) = NE.partition isRecFldGRE gres
    num_flds     = length flds
    num_non_flds = length non_flds

mkNameClashErr :: GlobalRdrEnv -> RdrName -> NE.NonEmpty GlobalRdrElt -> TcRnMessage
mkNameClashErr gre_env rdr_name gres = TcRnAmbiguousName gre_env rdr_name gres

dupNamesErr :: NE.NonEmpty SrcSpan -> NE.NonEmpty RdrName -> RnM ()
dupNamesErr locs names
  = addErrAt big_loc (TcRnBindingNameConflict (NE.head names) locs)
  where
    big_loc = foldr1 combineSrcSpans locs

badQualBndrErr :: RdrName -> TcRnMessage
badQualBndrErr rdr_name = TcRnQualifiedBinder rdr_name

typeAppErr :: TypeOrKind -> LHsType GhcPs -> TcRnMessage
typeAppErr what (L _ k) = TcRnTypeApplicationsDisabled (TypeApplication k what)

badFieldConErr :: Name -> FieldLabelString -> TcRnMessage
badFieldConErr con field = TcRnInvalidRecordField con field

-- | Ensure that a boxed or unboxed tuple has arity no larger than
-- 'mAX_TUPLE_SIZE'.
checkTupSize :: Int -> TcM ()
checkTupSize tup_size
  | tup_size <= mAX_TUPLE_SIZE
  = return ()
  | otherwise
  = addErr (TcRnTupleTooLarge tup_size)

-- | Ensure that a constraint tuple has arity no larger than 'mAX_CTUPLE_SIZE'.
checkCTupSize :: Int -> TcM ()
checkCTupSize tup_size
  | tup_size <= mAX_CTUPLE_SIZE
  = return ()
  | otherwise
  = addErr (TcRnCTupleTooLarge tup_size)

{- *********************************************************************
*                                                                      *
              Generating code for HsExpanded
      See Note [Handling overloaded and rebindable constructs]
*                                                                      *
********************************************************************* -}

wrapGenSpan :: a -> LocatedAn an a
-- Wrap something in a "generatedSrcSpan"
-- See Note [Rebindable syntax and HsExpansion]
wrapGenSpan x = L (noAnnSrcSpan generatedSrcSpan) x

genHsApps :: Name -> [LHsExpr GhcRn] -> HsExpr GhcRn
genHsApps fun args = foldl genHsApp (genHsVar fun) args

genHsApp :: HsExpr GhcRn -> LHsExpr GhcRn -> HsExpr GhcRn
genHsApp fun arg = HsApp noAnn (wrapGenSpan fun) arg

genLHsApp :: HsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
genLHsApp fun arg = wrapGenSpan (genHsApp fun arg)

genLHsVar :: Name -> LHsExpr GhcRn
genLHsVar nm = wrapGenSpan $ genHsVar nm

genHsVar :: Name -> HsExpr GhcRn
genHsVar nm = HsVar noExtField $ wrapGenSpan nm

genAppType :: HsExpr GhcRn -> HsType (NoGhcTc GhcRn) -> HsExpr GhcRn
genAppType expr ty = HsAppType noExtField (wrapGenSpan expr) noHsTok (mkEmptyWildCardBndrs (wrapGenSpan ty))

genLHsLit :: HsLit GhcRn -> LocatedAn an (HsExpr GhcRn)
genLHsLit = wrapGenSpan . HsLit noAnn

genHsIntegralLit :: IntegralLit -> LocatedAn an (HsExpr GhcRn)
genHsIntegralLit = genLHsLit . HsInt noExtField

genHsTyLit :: FastString -> HsType GhcRn
genHsTyLit = HsTyLit noExtField . HsStrTy NoSourceText

genSimpleConPat :: Name -> [LPat GhcRn] -> LPat GhcRn
-- The pattern (C p1 .. pn)
genSimpleConPat con pats
  = wrapGenSpan $ ConPat { pat_con_ext = noExtField
                         , pat_con     = wrapGenSpan con
                         , pat_args    = PrefixCon [] pats }

genVarPat :: Name -> LPat GhcRn
genVarPat n = wrapGenSpan $ VarPat noExtField (wrapGenSpan n)

genWildPat :: LPat GhcRn
genWildPat = wrapGenSpan $ WildPat noExtField

genSimpleFunBind :: Name -> [LPat GhcRn]
                 -> LHsExpr GhcRn -> LHsBind GhcRn
genSimpleFunBind fun pats expr
  = L gen $ genFunBind (L gen fun)
        [mkMatch (mkPrefixFunRhs (L gen fun)) pats expr
                 emptyLocalBinds]
  where
    gen = noAnnSrcSpan generatedSrcSpan

genFunBind :: LocatedN Name -> [LMatch GhcRn (LHsExpr GhcRn)]
           -> HsBind GhcRn
genFunBind fn ms
  = FunBind { fun_id = fn
            , fun_matches = mkMatchGroup Generated (wrapGenSpan ms)
            , fun_ext = emptyNameSet
            }

isIrrefutableHsPatRn :: forall p. (OutputableBndrId p)
                  => DynFlags -> LPat (GhcPass p) -> Bool
isIrrefutableHsPatRn dflags =
    isIrrefutableHsPat (xopt LangExt.Strict dflags)
