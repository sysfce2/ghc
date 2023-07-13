{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Domain types used in "GHC.HsToCore.Pmc.Solver".
-- The ultimate goal is to define 'Nabla', which models normalised refinement
-- types from the paper
-- [Lower Your Guards: A Compositional Pattern-Match Coverage Checker"](https://dl.acm.org/doi/abs/10.1145/3408989).
module GHC.HsToCore.Pmc.Solver.Types (

        -- * Normalised refinement types
        BotInfo(..), PmAltConApp(..), VarInfo(..), TmState(..), TmEGraph, TyState(..),
        Nabla(..), Nablas(..), initNablas,
        lookupRefuts, lookupSolution,

        -- ** Looking up 'VarInfo'
        lookupVarInfo, lookupVarInfoNT, trvVarInfo, emptyVarInfo,

        representId, representIds, representIdNablas, representIdsNablas,
        lookupMatchIdMap,

        -- ** Caching residual COMPLETE sets
        CompleteMatch, ResidualCompleteMatches(..), getRcm, isRcmInitialised,

        -- ** Representations for Literals and AltCons
        PmLit(..), PmLitValue(..), PmAltCon(..), pmLitType, pmAltConType,
        isPmAltConMatchStrict, pmAltConImplBangs,

        -- *** PmAltConSet
        PmAltConSet, emptyPmAltConSet, isEmptyPmAltConSet, elemPmAltConSet,
        extendPmAltConSet, pmAltConSetElems,

        -- *** Equality on 'PmAltCon's
        PmEquality(..), eqPmAltCon,

        -- *** Operations on 'PmLit'
        literalToPmLit, negatePmLit, overloadPmLit,
        pmLitAsStringLit, coreExprAsPmLit

    ) where

import GHC.Prelude

import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Types.Id
import GHC.Types.Unique.DSet
import GHC.Types.Name
import GHC.Core.Equality
import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Utils.Outputable
import GHC.Utils.Panic.Plain
import GHC.Utils.Misc (lastMaybe)
import GHC.Data.List.SetOps (unionLists)
import GHC.Data.Maybe
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Types.Literal
import GHC.Core
import GHC.Core.TyCo.Compare( eqType )
import GHC.Core.Map.Type
import GHC.Core.Utils (exprType)
import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Tc.Solver.InertSet (InertSet, emptyInert)
import GHC.Tc.Utils.TcType (isStringTy)
import GHC.Types.Var.Env
import GHC.Types.CompleteMatch (CompleteMatch(..))
import GHC.Types.SourceText (SourceText(..), mkFractionalLit, FractionalLit
                            , fractionalLitFromRational
                            , FractionalExponentBase(..))
import Numeric (fromRat)
import Data.Foldable (find)
import Data.Ratio
import GHC.Real (Ratio(..))
import qualified Data.Semigroup as Semi

import Data.Functor.Compose
import Data.Equality.Analysis (Analysis(..))
import Data.Equality.Graph (EGraph, ClassId)
import Data.Equality.Graph.Lens
import qualified Data.Equality.Graph as EG
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS (empty)
import Data.Bifunctor (second)
import Control.Monad.Trans.State (runState, state, execState)

-- import GHC.Driver.Ppr

--
-- * Normalised refinement types
--

-- | A normalised refinement type ∇ (\"nabla\"), comprised of an inert set of
-- canonical (i.e. mutually compatible) term and type constraints that form the
-- refinement type's predicate.
data Nabla
  = MkNabla
  { nabla_ty_st :: !TyState
  -- ^ Type oracle; things like a~Int
  , nabla_tm_st :: !TmState
  -- ^ Term oracle; things like x~Nothing
  }

-- | An initial nabla that is always satisfiable
initNabla :: Nabla
initNabla = MkNabla initTyState initTmState

instance Outputable Nabla where
  ppr nabla = hang (text "Nabla") 2 $ vcat [
      -- intentionally formatted this way enable the dev to comment in only
      -- the info they need
      ppr (nabla_tm_st nabla),
      ppr (nabla_ty_st nabla)
    ]

-- | A disjunctive bag of 'Nabla's, representing a refinement type.
newtype Nablas = MkNablas (Bag Nabla)

initNablas :: Nablas
initNablas = MkNablas (unitBag initNabla)

instance Outputable Nablas where
  ppr (MkNablas nablas) = ppr nablas

instance Semigroup Nablas where
  MkNablas l <> MkNablas r = MkNablas (l `unionBags` r)

instance Monoid Nablas where
  mempty = MkNablas emptyBag

-- | The type oracle state. An 'GHC.Tc.Solver.Monad.InertSet' that we
-- incrementally add local type constraints to, together with a sequence
-- number that counts the number of times we extended it with new facts.
data TyState = TySt { ty_st_n :: !Int, ty_st_inert :: !InertSet }

-- | Not user-facing.
instance Outputable TyState where
  ppr (TySt n inert) = ppr n <+> ppr inert

initTyState :: TyState
initTyState = TySt 0 emptyInert

-- | The term oracle state. Stores 'VarInfo' for encountered 'Id's and
-- 'CoreExpr's. These entries are possibly shared when we figure out that two
-- variables must be equal, thus represent the same set of values.
--
-- See Note [TmState invariants] in "GHC.HsToCore.Pmc.Solver".
data TmState
  = TmSt
  { ts_facts :: !TmEGraph
  -- ^ Facts about terms.
  , ts_reps :: !(IdEnv ClassId)
  -- ^ A mapping from match-id Ids to the class-id representing that match-id

  -- ROMES:TODO: ts_dirty looks a bit to me like the bookeeping needed to know
  -- which nodes to upward merge, perhaps we can get rid of it too.
  , ts_dirty :: !IntSet
  -- ^ Which 'VarInfo' needs to be checked for inhabitants because of new
  -- negative constraints (e.g. @x ≁ ⊥@ or @x ≁ K@).
  }

lookupMatchIdMap :: Id -> Nabla -> Maybe ClassId
lookupMatchIdMap id (MkNabla _ TmSt{ts_reps}) = lookupVarEnv ts_reps id

-- | Information about an 'Id'. Stores positive ('vi_pos') facts, like @x ~ Just 42@,
-- and negative ('vi_neg') facts, like "x is not (:)".
-- Also caches the type ('vi_ty'), the 'ResidualCompleteMatches' of a COMPLETE set
-- ('vi_rcm').
--
-- Subject to Note [The Pos/Neg invariant] in "GHC.HsToCore.Pmc.Solver".
data VarInfo
  = VI
  { vi_id  :: !Id
  -- ^ The 'Id' in question. Important for adding new constraints relative to
  -- this 'VarInfo' when we don't easily have the 'Id' available.
  -- ROMES:TODO: What is the Id in question when we might have multiple Ids in the same equivalence class?
  -- It seems currenlty this is the representative of the e-class, so we could probably drop it, in favour of Type or so (since sometimes we need to know the type, and that's also reasonable data for the e-class to have)

  , vi_pos :: ![PmAltConApp]
  -- ^ Positive info: 'PmAltCon' apps it is (i.e. @x ~ [Just y, PatSyn z]@), all
  -- at the same time (i.e. conjunctive).  We need a list because of nested
  -- pattern matches involving pattern synonym
  --    case x of { Just y -> case x of PatSyn z -> ... }
  -- However, no more than one RealDataCon in the list, otherwise contradiction
  -- because of generativity (which would violate Invariant 1 from the paper).

  , vi_neg :: !PmAltConSet
  -- ^ Negative info: A list of 'PmAltCon's that it cannot match.
  -- Example, assuming
  --
  -- @
  --     data T = Leaf Int | Branch T T | Node Int T
  -- @
  --
  -- then @x ≁ [Leaf, Node]@ means that @x@ cannot match a @Leaf@ or @Node@,
  -- and hence can only match @Branch@. Is orthogonal to anything from 'vi_pos',
  -- in the sense that 'eqPmAltCon' returns @PossiblyOverlap@ for any pairing
  -- between 'vi_pos' and 'vi_neg'.

  -- See Note [Why record both positive and negative info?]
  -- It's worth having an actual set rather than a simple association list,
  -- because files like Cabal's `LicenseId` define relatively huge enums
  -- that lead to quadratic or worse behavior.

  , vi_bot :: BotInfo
  -- ^ Can this variable be ⊥? Models (mutually contradicting) @x ~ ⊥@ and
  --   @x ≁ ⊥@ constraints. E.g.
  --    * 'MaybeBot': Don't know; Neither @x ~ ⊥@ nor @x ≁ ⊥@.
  --    * 'IsBot': @x ~ ⊥@
  --    * 'IsNotBot': @x ≁ ⊥@

  , vi_rcm :: !ResidualCompleteMatches
  -- ^ A cache of the associated COMPLETE sets. At any time a superset of
  -- possible constructors of each COMPLETE set. So, if it's not in here, we
  -- can't possibly match on it. Complementary to 'vi_neg'. We still need it
  -- to recognise completion of a COMPLETE set efficiently for large enums.
  }

data PmAltConApp
  = PACA
  { paca_con :: !PmAltCon
  , paca_tvs :: ![TyVar]
  , paca_ids :: ![ClassId]
  }

-- | See 'vi_bot'.
data BotInfo
  = IsBot
  | IsNotBot
  | MaybeBot
  deriving Eq

instance Outputable PmAltConApp where
  ppr PACA{paca_con = con, paca_tvs = tvs, paca_ids = ids} =
    hsep (ppr con : map ((char '@' <>) . ppr) tvs ++ map ppr ids)

instance Outputable BotInfo where
  ppr MaybeBot = underscore
  ppr IsBot    = text "~⊥"
  ppr IsNotBot = text "≁⊥"

instance Outputable IntSet where
  ppr = text . show

-- | Not user-facing.
instance Outputable TmState where
  ppr (TmSt eg idmp dirty) = text (show eg) $$ ppr idmp $$ ppr dirty

-- | Not user-facing.
instance Outputable VarInfo where
  ppr (VI x pos neg bot cache)
    = braces (hcat (punctuate comma [pp_x, pp_pos, pp_neg, ppr bot, pp_cache]))
    where
      pp_x = ppr x <> dcolon <> ppr (idType x)
      pp_pos
        | [] <- pos  = underscore
        | [p] <- pos = char '~' <> ppr p -- suppress outer [_] if singleton
        | otherwise  = char '~' <> ppr pos
      pp_neg
        | isEmptyPmAltConSet neg = underscore
        | otherwise              = char '≁' <> ppr neg
      pp_cache
        | RCM Nothing Nothing <- cache = underscore
        | otherwise                    = ppr cache

-- | Initial state of the term oracle.
initTmState :: TmState
initTmState = TmSt EG.emptyEGraph mempty IS.empty

-- | A data type that caches for the 'VarInfo' of @x@ the results of querying
-- 'dsGetCompleteMatches' and then striking out all occurrences of @K@ for
-- which we already know @x ≁ K@ from these sets.
--
-- For motivation, see Section 5.3 in Lower Your Guards.
-- See also Note [Implementation of COMPLETE pragmas]
data ResidualCompleteMatches
  = RCM
  { rcm_vanilla :: !(Maybe CompleteMatch)
  -- ^ The residual set for the vanilla COMPLETE set from the data defn.
  -- Tracked separately from 'rcm_pragmas', because it might only be
  -- known much later (when we have enough type information to see the 'TyCon'
  -- of the match), or not at all even. Until that happens, it is 'Nothing'.
  , rcm_pragmas :: !(Maybe [CompleteMatch])
  -- ^ The residual sets for /all/ COMPLETE sets from pragmas that are
  -- visible when compiling this module. Querying that set with
  -- 'dsGetCompleteMatches' requires 'DsM', so we initialise it with 'Nothing'
  -- until first needed in a 'DsM' context.
  }

getRcm :: ResidualCompleteMatches -> [CompleteMatch]
getRcm (RCM vanilla pragmas) = maybeToList vanilla ++ fromMaybe [] pragmas

isRcmInitialised :: ResidualCompleteMatches -> Bool
isRcmInitialised (RCM vanilla pragmas) = isJust vanilla && isJust pragmas

instance Outputable ResidualCompleteMatches where
  -- formats as "[{Nothing,Just},{P,Q}]"
  ppr rcm = ppr (getRcm rcm)

-----------------------
-- * Looking up VarInfo

emptyRCM :: ResidualCompleteMatches
emptyRCM = RCM Nothing Nothing

emptyVarInfo :: Id -> VarInfo
emptyVarInfo x
  = VI
  { vi_id = x
  , vi_pos = []
  , vi_neg = emptyPmAltConSet
  -- Why not set IsNotBot for unlifted type here?
  -- Because we'd have to trigger an inhabitation test, which we can't.
  -- See case (4) in Note [Strict fields and variables of unlifted type]
  -- in GHC.HsToCore.Pmc.Solver
  , vi_bot = MaybeBot
  , vi_rcm = emptyRCM
  }

-- | @lookupVarInfo tms x@ tells what we know about 'x'
--- romes:TODO: This will have a different type. I don't know what yet.
-- romes:TODO I don't think this is what we want any longer, more like represent Id and see if it was previously represented by some data or not?
lookupVarInfo :: TmState -> ClassId -> VarInfo
lookupVarInfo (TmSt eg _ _) x
-- RM: Yea, I don't like the fact that currently all e-classes are created by Ids and have an Empty Var info, yet we must call "fromMaybe" here. Not good.
  = eg ^._class x._data

-- | Like @lookupVarInfo ts x@, but @lookupVarInfo ts x = (y, vi)@ also looks
-- through newtype constructors. We have @x ~ N1 (... (Nk y))@ such that the
-- returned @y@ doesn't have a positive newtype constructor constraint
-- associated with it (yet). The 'VarInfo' returned is that of @y@'s
-- representative.
--
-- Careful, this means that @idType x@ might be different to @idType y@, even
-- modulo type normalisation!
--
-- See also Note [Coverage checking Newtype matches] in GHC.HsToCore.Pmc.Solver.
--
-- RM: looks like we could get perhaps represent the newtypes in the e-graph instead and somehow simplify this?
lookupVarInfoNT :: TmState -> ClassId -> (ClassId, VarInfo)
lookupVarInfoNT ts x = case lookupVarInfo ts x of
  VI{ vi_pos = as_newtype -> Just y } -> lookupVarInfoNT ts y
  res -> (x, res)
  where
    as_newtype = listToMaybe . mapMaybe go
    go PACA{paca_con = PmAltConLike (RealDataCon dc), paca_ids = [y]}
      | isNewDataCon dc = Just y
    go _                = Nothing

-- romes: We could probably inline this
trvVarInfo :: forall f a. Functor f => (VarInfo -> f (a,VarInfo)) -> Nabla -> ClassId -> f (a,Nabla)
trvVarInfo f nabla@MkNabla{ nabla_tm_st = ts@TmSt{ts_facts = env} } x
  = second (\g -> nabla{nabla_tm_st = ts{ts_facts=g}}) <$> updateAccum (_class x._data) f env
    where
      updateAccum :: forall f a s c. Functor f => Lens' s a -> (a -> f (c,a)) -> s -> f (c,s)
      updateAccum lens g = getCompose . lens @(Compose f ((,) c)) (Compose . g)

------------------------------------------------
-- * Exported utility functions querying 'Nabla'

-- ROMES:TODO: Document
-- | Lookup the refutable patterns, i.e. the pattern alt cons that certainly can't happen??
-- ROMES:TODO: ClassId?
lookupRefuts :: Nabla -> ClassId -> [PmAltCon]
-- Unfortunately we need the extra bit of polymorphism and the unfortunate
-- duplication of lookupVarInfo here.
lookupRefuts MkNabla{ nabla_tm_st = ts } x =
  pmAltConSetElems $ vi_neg $ lookupVarInfo ts x

isDataConSolution :: PmAltConApp -> Bool
isDataConSolution PACA{paca_con = PmAltConLike (RealDataCon _)} = True
isDataConSolution _                                             = False

-- @lookupSolution nabla x@ picks a single solution ('vi_pos') of @x@ from
-- possibly many, preferring 'RealDataCon' solutions whenever possible.
lookupSolution :: Nabla -> ClassId -> Maybe PmAltConApp
lookupSolution nabla x = case vi_pos (lookupVarInfo (nabla_tm_st nabla) x) of
  []                                         -> Nothing
  pos@(x:_)
    | Just sol <- find isDataConSolution pos -> Just sol
    | otherwise                              -> Just x

--------------------------------------------------------------------------------
-- The rest is just providing an IR for (overloaded!) literals and AltCons that
-- sits between Hs and Core. We need a reliable way to detect and determine
-- equality between them, which is impossible with Hs (too expressive) and with
-- Core (no notion of overloaded literals, and even plain 'Int' literals are
-- actually constructor apps). Also String literals are troublesome.

-- | Literals (simple and overloaded ones) for pattern match checking.
--
-- See Note [Undecidable Equality for PmAltCons]
data PmLit = PmLit
           { pm_lit_ty  :: Type
           , pm_lit_val :: PmLitValue }

data PmLitValue
  = PmLitInt Integer
  | PmLitRat Rational
  | PmLitChar Char
  -- We won't actually see PmLitString in the oracle since we desugar strings to
  -- lists
  | PmLitString FastString
  | PmLitOverInt Int {- How often Negated? -} Integer
  | PmLitOverRat Int {- How often Negated? -} FractionalLit
  | PmLitOverString FastString

-- | Undecidable semantic equality result.
-- See Note [Undecidable Equality for PmAltCons]
data PmEquality
  = Equal
  | Disjoint
  | PossiblyOverlap
  deriving (Eq, Show)

-- | When 'PmEquality' can be decided. @True <=> Equal@, @False <=> Disjoint@.
decEquality :: Bool -> PmEquality
decEquality True  = Equal
decEquality False = Disjoint

-- | Undecidable equality for values represented by 'PmLit's.
-- See Note [Undecidable Equality for PmAltCons]
--
-- * @Just True@ ==> Surely equal
-- * @Just False@ ==> Surely different (non-overlapping, even!)
-- * @Nothing@ ==> Equality relation undecidable
eqPmLit :: PmLit -> PmLit -> PmEquality
eqPmLit (PmLit t1 v1) (PmLit t2 v2)
  -- no haddock | pprTrace "eqPmLit" (ppr t1 <+> ppr v1 $$ ppr t2 <+> ppr v2) False = undefined
  | not (t1 `eqType` t2) = Disjoint
  | otherwise            = go v1 v2
  where
    go (PmLitInt i1)        (PmLitInt i2)        = decEquality (i1 == i2)
    go (PmLitRat r1)        (PmLitRat r2)        = decEquality (r1 == r2)
    go (PmLitChar c1)       (PmLitChar c2)       = decEquality (c1 == c2)
    go (PmLitString s1)     (PmLitString s2)     = decEquality (s1 == s2)
    go (PmLitOverInt n1 i1) (PmLitOverInt n2 i2)
      | n1 == n2 && i1 == i2                     = Equal
    go (PmLitOverRat n1 r1) (PmLitOverRat n2 r2)
      | n1 == n2 && r1 == r2                     = Equal
    go (PmLitOverString s1) (PmLitOverString s2)
      | s1 == s2                                 = Equal
    go _                    _                    = PossiblyOverlap

-- | Syntactic equality.
instance Eq PmLit where
  a == b = eqPmLit a b == Equal

-- | Type of a 'PmLit'
pmLitType :: PmLit -> Type
pmLitType (PmLit ty _) = ty

-- | Undecidable equality for values represented by 'ConLike's.
-- See Note [Undecidable Equality for PmAltCons].
-- 'PatSynCon's aren't enforced to be generative, so two syntactically different
-- 'PatSynCon's might match the exact same values. Without looking into and
-- reasoning about the pattern synonym's definition, we can't decide if their
-- sets of matched values is different.
--
-- * @Just True@ ==> Surely equal
-- * @Just False@ ==> Surely different (non-overlapping, even!)
-- * @Nothing@ ==> Equality relation undecidable
eqConLike :: ConLike -> ConLike -> PmEquality
eqConLike (RealDataCon dc1) (RealDataCon dc2) = decEquality (dc1 == dc2)
eqConLike (PatSynCon psc1)  (PatSynCon psc2)
  | psc1 == psc2
  = Equal
eqConLike _                 _                 = PossiblyOverlap

-- | Represents the head of a match against a 'ConLike' or literal.
-- Really similar to 'GHC.Core.AltCon'.
data PmAltCon = PmAltConLike ConLike
              | PmAltLit     PmLit

data PmAltConSet = PACS !(UniqDSet ConLike) ![PmLit]

emptyPmAltConSet :: PmAltConSet
emptyPmAltConSet = PACS emptyUniqDSet []

isEmptyPmAltConSet :: PmAltConSet -> Bool
isEmptyPmAltConSet (PACS cls lits) = isEmptyUniqDSet cls && null lits

-- | Whether there is a 'PmAltCon' in the 'PmAltConSet' that compares 'Equal' to
-- the given 'PmAltCon' according to 'eqPmAltCon'.
elemPmAltConSet :: PmAltCon -> PmAltConSet -> Bool
elemPmAltConSet (PmAltConLike cl) (PACS cls _   ) = elementOfUniqDSet cl cls
elemPmAltConSet (PmAltLit lit)    (PACS _   lits) = elem lit lits

extendPmAltConSet :: PmAltConSet -> PmAltCon -> PmAltConSet
extendPmAltConSet (PACS cls lits) (PmAltConLike cl)
  = PACS (addOneToUniqDSet cls cl) lits
extendPmAltConSet (PACS cls lits) (PmAltLit lit)
  = PACS cls (unionLists lits [lit])

-- | The elements of a 'PmAltConSet'
pmAltConSetElems :: PmAltConSet -> [PmAltCon]
pmAltConSetElems (PACS cls lits)
  = map PmAltConLike (uniqDSetToList cls) ++ map PmAltLit lits

instance Outputable PmAltConSet where
  ppr = ppr . pmAltConSetElems

-- | We can't in general decide whether two 'PmAltCon's match the same set of
-- values. In addition to the reasons in 'eqPmLit' and 'eqConLike', a
-- 'PmAltConLike' might or might not represent the same value as a 'PmAltLit'.
-- See Note [Undecidable Equality for PmAltCons].
--
-- * @Just True@ ==> Surely equal
-- * @Just False@ ==> Surely different (non-overlapping, even!)
-- * @Nothing@ ==> Equality relation undecidable
--
-- Examples (omitting some constructor wrapping):
--
-- * @eqPmAltCon (LitInt 42) (LitInt 1) == Just False@: Lit equality is
--   decidable
-- * @eqPmAltCon (DataCon A) (DataCon B) == Just False@: DataCon equality is
--   decidable
-- * @eqPmAltCon (LitOverInt 42) (LitOverInt 1) == Nothing@: OverLit equality
--   is undecidable
-- * @eqPmAltCon (PatSyn PA) (PatSyn PB) == Nothing@: PatSyn equality is
--   undecidable
-- * @eqPmAltCon (DataCon I#) (LitInt 1) == Nothing@: DataCon to Lit
--   comparisons are undecidable without reasoning about the wrapped @Int#@
-- * @eqPmAltCon (LitOverInt 1) (LitOverInt 1) == Just True@: We assume
--   reflexivity for overloaded literals
-- * @eqPmAltCon (PatSyn PA) (PatSyn PA) == Just True@: We assume reflexivity
--   for Pattern Synonyms
eqPmAltCon :: PmAltCon -> PmAltCon -> PmEquality
eqPmAltCon (PmAltConLike cl1) (PmAltConLike cl2) = eqConLike cl1 cl2
eqPmAltCon (PmAltLit     l1)  (PmAltLit     l2)  = eqPmLit l1 l2
eqPmAltCon _                  _                  = PossiblyOverlap

-- | Syntactic equality.
instance Eq PmAltCon where
  a == b = eqPmAltCon a b == Equal

-- | Type of a 'PmAltCon'
pmAltConType :: PmAltCon -> [Type] -> Type
pmAltConType (PmAltLit lit)     _arg_tys = assert (null _arg_tys ) $ pmLitType lit
pmAltConType (PmAltConLike con) arg_tys  = conLikeResTy con arg_tys

-- | Is a match on this constructor forcing the match variable?
-- True of data constructors, literals and pattern synonyms (#17357), but not of
-- newtypes.
-- See Note [Coverage checking Newtype matches] in GHC.HsToCore.Pmc.Solver.
isPmAltConMatchStrict :: PmAltCon -> Bool
isPmAltConMatchStrict PmAltLit{}                      = True
isPmAltConMatchStrict (PmAltConLike PatSynCon{})      = True -- #17357
isPmAltConMatchStrict (PmAltConLike (RealDataCon dc)) = not (isNewDataCon dc)

pmAltConImplBangs :: PmAltCon -> [HsImplBang]
pmAltConImplBangs PmAltLit{}         = []
pmAltConImplBangs (PmAltConLike con) = conLikeImplBangs con

{- Note [Undecidable Equality for PmAltCons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Equality on overloaded literals is undecidable in the general case. Consider
the following example:

  instance Num Bool where
    ...
    fromInteger 0 = False -- C-like representation of booleans
    fromInteger _ = True

    f :: Bool -> ()
    f 1 = ()        -- Clause A
    f 2 = ()        -- Clause B

Clause B is redundant but to detect this, we must decide the constraint:
@fromInteger 2 ~ fromInteger 1@ which means that we
have to look through function @fromInteger@, whose implementation could
be anything. This poses difficulties for:

1. The expressive power of the check.
   We cannot expect a reasonable implementation of pattern matching to detect
   that @fromInteger 2 ~ fromInteger 1@ is True, unless we unfold function
   fromInteger. This puts termination at risk and is undecidable in the
   general case.

2. Error messages/Warnings.
   What should our message for @f@ above be? A reasonable approach would be
   to issue:

     Pattern matches are (potentially) redundant:
       f 2 = ...    under the assumption that 1 == 2

   but seems to complex and confusing for the user.

We choose to equate only obviously equal overloaded literals, in all other cases
we signal undecidability by returning Nothing from 'eqPmAltCons'. We do
better for non-overloaded literals, because we know their fromInteger/fromString
implementation is actually injective, allowing us to simplify the constraint
@fromInteger 1 ~ fromInteger 2@ to @1 ~ 2@, which is trivially unsatisfiable.

The impact of this treatment of overloaded literals is the following:

  * Redundancy checking is rather conservative, since it cannot see that clause
    B above is redundant.

  * We have instant equality check for overloaded literals (we do not rely on
    the term oracle which is rather expensive, both in terms of performance and
    memory). This significantly improves the performance of functions `covered`
    `uncovered` and `divergent` in "GHC.HsToCore.Pmc" and effectively addresses
    #11161.

  * The warnings issued are simpler.

Similar reasoning applies to pattern synonyms: In contrast to data constructors,
which are generative, constraints like F a ~ G b for two different pattern
synonyms F and G aren't immediately unsatisfiable. We assume F a ~ F a, though.
-}

literalToPmLit :: Type -> Literal -> Maybe PmLit
literalToPmLit ty l = PmLit ty <$> go l
  where
    go (LitChar c)       = Just (PmLitChar c)
    go (LitFloat r)      = Just (PmLitRat r)
    go (LitDouble r)     = Just (PmLitRat r)
    go (LitString s)     = Just (PmLitString (mkFastStringByteString s))
    go (LitNumber _ i)   = Just (PmLitInt i)
    go _                 = Nothing

negatePmLit :: PmLit -> Maybe PmLit
negatePmLit (PmLit ty v) = PmLit ty <$> go v
  where
    go (PmLitInt i)       = Just (PmLitInt (-i))
    go (PmLitRat r)       = Just (PmLitRat (-r))
    go (PmLitOverInt n i) = Just (PmLitOverInt (n+1) i)
    go (PmLitOverRat n r) = Just (PmLitOverRat (n+1) r)
    go _                  = Nothing

overloadPmLit :: Type -> PmLit -> Maybe PmLit
overloadPmLit ty (PmLit _ v) = PmLit ty <$> go v
  where
    go (PmLitInt i)          = Just (PmLitOverInt 0 i)
    go (PmLitRat r)          = Just $! PmLitOverRat 0 $! fractionalLitFromRational r
    go (PmLitString s)
      | ty `eqType` stringTy = Just v
      | otherwise            = Just (PmLitOverString s)
    go ovRat@PmLitOverRat{}  = Just ovRat
    go _               = Nothing

pmLitAsStringLit :: PmLit -> Maybe FastString
pmLitAsStringLit (PmLit _ (PmLitString s)) = Just s
pmLitAsStringLit _                         = Nothing

coreExprAsPmLit :: CoreExpr -> Maybe PmLit
-- coreExprAsPmLit e | pprTrace "coreExprAsPmLit" (ppr e) False = undefined
coreExprAsPmLit (Tick _t e) = coreExprAsPmLit e
coreExprAsPmLit (Lit l) = literalToPmLit (literalType l) l
coreExprAsPmLit e = case collectArgs e of
  (Var x, [Lit l])
    | Just dc <- isDataConWorkId_maybe x
    , dc `elem` [intDataCon, wordDataCon, charDataCon, floatDataCon, doubleDataCon]
    -> literalToPmLit (exprType e) l
  (Var x, [Lit (LitNumber _ l)])
    | Just (ty,l) <- bignum_lit_maybe x l
    -> Just (PmLit ty (PmLitInt l))
  (Var x, [_ty, n_arg, d_arg])
    | Just dc <- isDataConWorkId_maybe x
    , dataConName dc == ratioDataConName
    , Just (PmLit _ (PmLitInt n)) <- coreExprAsPmLit n_arg
    , Just (PmLit _ (PmLitInt d)) <- coreExprAsPmLit d_arg
    -- HACK: just assume we have a literal double. This case only occurs for
    --       overloaded lits anyway, so we immediately override type information
    -> literalToPmLit (exprType e) (mkLitDouble (n % d))

  (Var x, args)
    -- See Note [Detecting overloaded literals with -XRebindableSyntax]
    | is_rebound_name x fromIntegerName
    , Just arg <- lastMaybe args
    , Just (_ty,l) <- bignum_conapp_maybe arg
    -> Just (PmLit integerTy (PmLitInt l)) >>= overloadPmLit (exprType e)
  (Var x, args)
    -- See Note [Detecting overloaded literals with -XRebindableSyntax]
    -- fromRational <expr>
    | is_rebound_name x fromRationalName
    , [r] <- dropWhile (not . is_ratio) args
    -> coreExprAsPmLit r >>= overloadPmLit (exprType e)

  --Rationals with large exponents
  (Var x, args)
    -- See Note [Detecting overloaded literals with -XRebindableSyntax]
    -- See Note [Dealing with rationals with large exponents]
    -- mkRationalBase* <rational> <exponent>
    | Just exp_base <- is_larg_exp_ratio x
    , [r, exp] <- dropWhile (not . is_ratio) args
    , (Var x, [_ty, n_arg, d_arg]) <- collectArgs r
    , Just dc <- isDataConWorkId_maybe x
    , dataConName dc == ratioDataConName
    , Just (PmLit _ (PmLitInt n)) <- coreExprAsPmLit n_arg
    , Just (PmLit _ (PmLitInt d)) <- coreExprAsPmLit d_arg
    , Just (_exp_ty,exp') <- bignum_conapp_maybe exp
    -> do
      let rational = (abs n) :% d
      let neg = if n < 0 then 1 else 0
      let frac = mkFractionalLit NoSourceText False rational exp' exp_base
      Just $ PmLit (exprType e) (PmLitOverRat neg frac)

  (Var x, args)
    | is_rebound_name x fromStringName
    -- See Note [Detecting overloaded literals with -XRebindableSyntax]
    , s:_ <- filter (isStringTy . exprType) $ filter isValArg args
    -- NB: Calls coreExprAsPmLit and then overloadPmLit, so that we return PmLitOverStrings
    -> coreExprAsPmLit s >>= overloadPmLit (exprType e)
  -- These last two cases handle proper String literals
  (Var x, [Type ty])
    | Just dc <- isDataConWorkId_maybe x
    , dc == nilDataCon
    , ty `eqType` charTy
    -> literalToPmLit stringTy (mkLitString "")
  (Var x, [Lit l])
    | idName x `elem` [unpackCStringName, unpackCStringUtf8Name]
    -> literalToPmLit stringTy l

  _ -> Nothing
  where
    bignum_conapp_maybe (App (Var x) (Lit (LitNumber _ l)))
      = bignum_lit_maybe x l
    bignum_conapp_maybe _ = Nothing

    bignum_lit_maybe x l
      | Just dc <- isDataConWorkId_maybe x
      = if | dc == integerISDataCon -> Just (integerTy,l)
           | dc == integerIPDataCon -> Just (integerTy,l)
           | dc == integerINDataCon -> Just (integerTy,negate l)
           | dc == naturalNSDataCon -> Just (naturalTy,l)
           | dc == naturalNBDataCon -> Just (naturalTy,l)
           | otherwise              -> Nothing
    bignum_lit_maybe _ _ = Nothing

    is_ratio (Type _) = False
    is_ratio r
      | Just (tc, _) <- splitTyConApp_maybe (exprType r)
      = tyConName tc == ratioTyConName
      | otherwise
      = False
    is_larg_exp_ratio x
      | is_rebound_name x mkRationalBase10Name
      = Just Base10
      | is_rebound_name x mkRationalBase2Name
      = Just Base2
      | otherwise
      = Nothing


    -- See Note [Detecting overloaded literals with -XRebindableSyntax]
    is_rebound_name :: Id -> Name -> Bool
    is_rebound_name x n = getOccFS (idName x) == getOccFS n

{- Note [Detecting overloaded literals with -XRebindableSyntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normally, we'd find e.g. overloaded string literals by comparing the
application head of an expression to `fromStringName`. But that doesn't work
with -XRebindableSyntax: The `Name` of a user-provided `fromString` function is
different to `fromStringName`, which lives in a certain module, etc.

There really is no other way than to compare `OccName`s and guess which
argument is the actual literal string (we assume it's the first argument of
type `String`).

The same applies to other overloaded literals, such as overloaded rationals
(`fromRational`)and overloaded integer literals (`fromInteger`).

Note [Dealing with rationals with large exponents]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Rationals with large exponents are *not* desugared to
a simple rational. As that would require us to compute
their value which can be expensive. Rather they desugar
to an expression. For example 1e1000 will desugar to an
expression of the form: `mkRationalWithExponentBase10 (1 :% 1) 1000`

Only overloaded literals desugar to this form however, so we
we can just return a overloaded rational literal.

The most complex case is if we have RebindableSyntax enabled.
By example if we have a pattern like this: `f 3.3 = True`

It will desugar to:
  fromRational
    [TYPE: Rational, mkRationalBase10 (:% @Integer 10 1) (-1)]

The fromRational is properly detected as an overloaded Rational by
coreExprAsPmLit and it's general code for detecting overloaded rationals.
See Note [Detecting overloaded literals with -XRebindableSyntax].

This case then recurses into coreExprAsPmLit passing only the expression
`mkRationalBase10 (:% @Integer 10 1) (-1)`. Which is caught by rationals
with large exponents case. This will return a `PmLitOverRat` literal.

Which is then passed to overloadPmLit which simply returns it as-is since
it's already overloaded.

-}

instance Outputable PmLitValue where
  ppr (PmLitInt i)        = ppr i
  ppr (PmLitRat r)        = double (fromRat r) -- good enough
  ppr (PmLitChar c)       = pprHsChar c
  ppr (PmLitString s)     = pprHsString s
  ppr (PmLitOverInt n i)  = minuses n (ppr i)
  ppr (PmLitOverRat n r)  = minuses n (ppr r)
  ppr (PmLitOverString s) = pprHsString s

-- Take care of negated literals
minuses :: Int -> SDoc -> SDoc
minuses n sdoc = iterate (\sdoc -> parens (char '-' <> sdoc)) sdoc !! n

instance Outputable PmLit where
  ppr (PmLit ty v) = ppr v <> suffix
    where
      -- Some ad-hoc hackery for displaying proper lit suffixes based on type
      tbl = [ (intPrimTy, primIntSuffix)
            , (int64PrimTy, primInt64Suffix)
            , (wordPrimTy, primWordSuffix)
            , (word64PrimTy, primWord64Suffix)
            , (charPrimTy, primCharSuffix)
            , (floatPrimTy, primFloatSuffix)
            , (doublePrimTy, primDoubleSuffix) ]
      suffix = maybe empty snd (find (eqType ty . fst) tbl)

instance Outputable PmAltCon where
  ppr (PmAltConLike cl) = ppr cl
  ppr (PmAltLit l)      = ppr l

instance Outputable PmEquality where
  ppr = text . show

--
-- * E-graphs to represent normalised refinment types
--

type TmEGraph = EGraph VarInfo (DeBruijnF CoreExprF)
-- TODO delete orphans for showing TmEGraph for debugging reasons
instance Show VarInfo where
  show = showPprUnsafe . ppr

-- | Represents a match-id in 'Nablas'
representIdNablas :: Id -> Nablas -> Nablas
representIdNablas x (MkNablas nbs) = MkNablas $ mapBag (snd . representId x) nbs

representIdsNablas :: [Id] -> Nablas -> Nablas
representIdsNablas xs = execState (mapM (\x -> state (((),) . representIdNablas x)) xs)

-- Are these even used? don't we always use the ones above?
-- | Like 'representId' but for a single Nabla
representId :: Id -> Nabla -> (ClassId, Nabla)
representId x (MkNabla tyst tmst@TmSt{ts_facts=eg0, ts_reps=idmp})
    = case lookupVarEnv idmp x of
        -- The reason why we can't use an empty new class is that we don't account for the IdMap in the 'representDBCoreExpr'
        -- In particular, if we represent "reverse @a xs" in the e-graph, the
        -- node in which "xs" will be represented won't match the e-class id
        -- representing "xs", because that class doesn't contain "VarF xs"
        -- Nothing -> case EG.newEClass (emptyVarInfo x) eg0 of
        Nothing -> case EG.add (EG.Node (DF (deBruijnize (VarF x)))) eg0 of
          (xid, eg1) -> (xid, MkNabla tyst tmst{ts_facts=eg1, ts_reps=extendVarEnv idmp x xid})
        Just xid -> (xid, MkNabla tyst tmst)

representIds :: [Id] -> Nabla -> ([ClassId], Nabla)
representIds xs = runState (mapM (state . representId) xs)

-- | This instance is seriously wrong for general purpose, it's just required for instancing Analysis.
-- There ought to be a better way.
instance Eq VarInfo where
  (==) a b = vi_id a == vi_id b
instance Analysis VarInfo (DeBruijnF CoreExprF) where
  {-# INLINE makeA #-}
  {-# INLINE joinA #-}

  -- When an e-class is created for a variable, we create an VarInfo from it.
  -- It doesn't matter if this variable is bound or free, since it's the first
  -- variable in this e-class (and all others would have to be equivalent to
  -- it)
  --
  -- Also, the Eq instance for DeBruijn Vars will ensure that two free
  -- variables with the same Id are equal and so they will be represented in
  -- the same e-class
  makeA (DF (D _ (VarF x))) = emptyVarInfo x
  makeA _ = emptyVarInfo unitDataConId -- ROMES:TODO: this is dummy information which should never be used, this is quite wrong :)
                                       -- I think the reason we end up in this
                                       -- situation is bc we first represent an expression and only then merge it with some Id.
                                       -- we'd need a way to represent directly into an e-class then, to not trigger the new e-class.

  -- romes: so currently, variables are joined in 'addVarCt' manually by getting the old value of $x$ and assuming the value of $y$ was chosen.
  -- That's obviously bad now, it'd be much more clearer to do it here. It's just the nabla threading that's more trouble
  -- Hacks hacks hacks
  -- Do some "obvious" things in this merge, despite keeping all the nuanced
  -- joining operations in addVarCt. Some part of them will be redundant, but
  -- if we don't do the simple things here we might end up losing information
  -- when merging things through the e-graph outside of 'addVarCt'

-- I think we really need effects, because the operation is only well-defined
-- since it can fail when it is conflicting
-- and that would allow us to do the merge procedure correcly here instead of in addVarCt
-- we may be able to have Analysis (Effect VarInfo) (...)
  joinA a b = b{ vi_id = if vi_id b == unitDataConId && vi_id a /= unitDataConId then vi_id a else vi_id b
               , vi_pos = case (vi_pos a, vi_pos b) of
                            ([], []) -> []
                            ([], x) -> x
                            (x, []) -> x
                            (_x, y) -> y -- keep right
               , vi_neg = foldr (flip extendPmAltConSet) (vi_neg b) (pmAltConSetElems $ vi_neg a)
               , vi_bot = case (vi_bot a, vi_bot b) of
                            (IsBot,IsBot) -> IsBot
                            (IsBot,IsNotBot) -> IsNotBot -- keep b, achhhhh
                            (IsBot,MaybeBot) -> IsBot
                            (IsNotBot,IsBot) -> IsBot -- keep b, achhhhh
                            (IsNotBot,IsNotBot) -> IsNotBot
                            (IsNotBot,MaybeBot) -> IsNotBot
                            (MaybeBot, IsBot) -> IsBot
                            (MaybeBot, IsNotBot) -> IsNotBot
                            (MaybeBot, MaybeBot) -> MaybeBot
               , vi_rcm = case (vi_rcm a, vi_rcm b) of
                            (RCM Nothing Nothing,RCM a b) -> RCM a b
                            (RCM Nothing (Just a),RCM Nothing Nothing) -> RCM Nothing (Just a)
                            (RCM Nothing (Just _a),RCM Nothing (Just b)) -> RCM Nothing (Just b) -- keep right
                            (RCM Nothing (Just a),RCM (Just b) Nothing) -> RCM (Just b) (Just a)
                            (RCM Nothing (Just _a),RCM (Just b) (Just c)) -> RCM (Just b) (Just c) -- keep right
                            (RCM (Just a) Nothing,RCM Nothing Nothing) -> RCM (Just a) Nothing
                            (RCM (Just a) Nothing,RCM Nothing (Just b)) -> RCM (Just a) (Just b)
                            (RCM (Just _a) Nothing,RCM (Just b) Nothing) -> RCM (Just b) Nothing -- keep right
                            (RCM (Just _a) Nothing,RCM (Just b) (Just c)) -> RCM (Just b) (Just c)
                            (RCM (Just a) (Just b),RCM Nothing Nothing) -> RCM (Just a) (Just b)
                            (RCM (Just a) (Just _b),RCM Nothing (Just c)) -> RCM (Just a) (Just c)
                            (RCM (Just _a) (Just b),RCM (Just c) Nothing) -> RCM (Just c) (Just b)
                            (RCM (Just _a) (Just _b),RCM (Just c) (Just d)) -> RCM (Just c) (Just d)
                            -- we could also have _ _, (Just c) (Just d) -> (Just c, Just d)
               }

