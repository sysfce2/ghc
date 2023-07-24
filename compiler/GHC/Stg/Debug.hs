{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-- This module contains functions which implement
-- the -finfo-table-map and -fdistinct-constructor-tables flags
module GHC.Stg.Debug
  ( StgDebugOpts(..)
  , StgDebugDctConfig(..)
  , dctConfigPlus
  , dctConfigMinus
  , collectDebugInformation
  , parseDistinctConstructorTablesArg
  ) where

import GHC.Prelude

import GHC.Stg.Syntax

import GHC.Types.Id
import GHC.Types.Tickish
import GHC.Core.DataCon
import GHC.Types.IPE
import GHC.Unit.Module
import GHC.Types.Name   ( getName, getOccName, occNameFS, nameSrcSpan, occName, occNameString)
import GHC.Data.FastString

import Control.Monad (when)
import Control.Monad.Trans.Reader
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Utils.Monad.State.Strict
import GHC.Utils.Binary (Binary)
import qualified GHC.Utils.Binary as B
import Control.Monad.Trans.Class
import GHC.Types.Unique.Map
import GHC.Types.SrcLoc
import Control.Applicative
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

data SpanWithLabel = SpanWithLabel RealSrcSpan LexicalFastString

data StgDebugOpts = StgDebugOpts
  { stgDebug_infoTableMap              :: !Bool
  , stgDebug_distinctConstructorTables :: !StgDebugDctConfig
  }

-- | Configuration describing which constructors should be given distinct info
-- tables for each usage.
data StgDebugDctConfig =
    -- | Create distinct constructor tables for each usage of any data
    -- constructor.
    --
    -- This is the behavior if just @-fdistinct-constructor-tables@ is supplied.
    All

    -- | Create distinct constructor tables for each usage of only these data
    -- constructors.
    --
    -- This is the behavior if @-fdistinct-constructor-tables=C1,...,CN@ is
    -- supplied.
  | Only !(Set String)

    -- | Create distinct constructor tables for each usage of any data
    -- constructor except these ones.
    --
    -- This is the behavior if @-fdistinct-constructor-tables@ and
    -- @-fno-distinct-constructor-tables=C1,...,CN@ is given.
  | AllExcept !(Set String)

    -- | Do not create distinct constructor tables for any data constructor.
    --
    -- This is the behavior if no @-fdistinct-constructor-tables@ is given (or
    -- @-fno-distinct-constructor-tables@ is given).
  | None

-- | Necessary for 'StgDebugDctConfig' to be included in the dynflags
-- fingerprint
instance Binary StgDebugDctConfig where
  put_ bh All = B.putByte bh 0
  put_ bh (Only cs) = do
    B.putByte bh 1
    B.put_ bh cs
  put_ bh (AllExcept cs) = do
    B.putByte bh 2
    B.put_ bh cs
  put_ bh None = B.putByte bh 3

  get bh = do
    h <- B.getByte bh
    case h of
      0 -> pure All
      1 -> Only <$> B.get bh
      2 -> AllExcept <$> B.get bh
      _ -> pure None

-- | Given a distinct constructor tables configuration and a set of constructor
-- names that we want to generate distinct info tables for, create a new
-- configuration which includes those constructors.
--
-- If the given set is empty, that means the user has entered
-- @-fdistinct-constructor-tables@ with no constructor names specified, and
-- therefore we consider that an 'All' configuration.
dctConfigPlus :: StgDebugDctConfig -> Set String -> StgDebugDctConfig
dctConfigPlus cfg cs
    | Set.null cs = All
    | otherwise =
        case (cfg, cs) of
          (All            , _  ) -> All
          ((Only      cs1), cs2) -> Only $ Set.union cs1 cs2
          ((AllExcept cs1), cs2) -> AllExcept $ Set.difference cs1 cs2
          (None           , cs ) -> Only cs

-- | Given a distinct constructor tables configuration and a set of constructor
-- names that we /do not/ want to generate distinct info tables for, create a
-- new configuration which excludes those constructors.
--
-- If the given set is empty, that means the user has entered
-- @-fno-distinct-constructor-tables@ with no constructor names specified, and
-- therefore we consider that a 'None' configuration.
dctConfigMinus :: StgDebugDctConfig -> Set String -> StgDebugDctConfig
dctConfigMinus cfg cs
    | Set.null cs = None
    | otherwise =
        case (cfg, cs) of
          (All            , cs ) -> AllExcept cs
          ((Only      cs1), cs2) -> Only $ Set.difference cs1 cs2
          ((AllExcept cs1), cs2) -> AllExcept $ Set.union cs1 cs2
          (None           , _  ) -> None


data R = R { rOpts :: StgDebugOpts, rModLocation :: ModLocation, rSpan :: Maybe SpanWithLabel }

type M a = ReaderT R (State InfoTableProvMap) a

withSpan :: IpeSourceLocation -> M a -> M a
withSpan (new_s, new_l) act = local maybe_replace act
  where
    maybe_replace r@R{ rModLocation = cur_mod, rSpan = Just (SpanWithLabel old_s _old_l) }
      -- prefer spans from the current module
      | Just (unpackFS $ srcSpanFile old_s) == ml_hs_file cur_mod
      , Just (unpackFS $ srcSpanFile new_s) /= ml_hs_file cur_mod
      = r
    maybe_replace r
      = r { rSpan = Just (SpanWithLabel new_s new_l) }

collectDebugInformation :: StgDebugOpts -> ModLocation -> [StgTopBinding] -> ([StgTopBinding], InfoTableProvMap)
collectDebugInformation opts ml bs =
    runState (runReaderT (mapM collectTop bs) (R opts ml Nothing)) emptyInfoTableProvMap

collectTop :: StgTopBinding -> M StgTopBinding
collectTop (StgTopLifted t) = StgTopLifted <$> collectStgBind t
collectTop tb = return tb

collectStgBind :: StgBinding -> M StgBinding
collectStgBind  (StgNonRec bndr rhs) = do
    rhs' <- collectStgRhs bndr rhs
    return (StgNonRec bndr rhs')
collectStgBind (StgRec pairs) = do
    es <- mapM (\(b, e) -> (b,) <$> collectStgRhs b e) pairs
    return (StgRec es)

collectStgRhs :: Id -> StgRhs -> M StgRhs
collectStgRhs bndr (StgRhsClosure ext cc us bs e t) = do
  let
    name = idName bndr
    -- If the name has a span, use that initially as the source position in-case
    -- we don't get anything better.
    with_span = case nameSrcSpan name of
                  RealSrcSpan pos _ -> withSpan (pos, LexicalFastString $ occNameFS (getOccName name))
                  _ -> id
  e' <- with_span $ collectExpr e
  recordInfo bndr e'
  return $ StgRhsClosure ext cc us bs e' t
collectStgRhs _bndr (StgRhsCon cc dc _mn ticks args typ) = do
  n' <- numberDataCon dc ticks
  return (StgRhsCon cc dc n' ticks args typ)


recordInfo :: Id -> StgExpr -> M ()
recordInfo bndr new_rhs = do
  modLoc <- asks rModLocation
  let
    thisFile = maybe nilFS mkFastString $ ml_hs_file modLoc
    -- A span from the ticks surrounding the new_rhs
    best_span = quickSourcePos thisFile new_rhs
    -- A back-up span if the bndr had a source position, many do not (think internally generated ids)
    bndr_span = (\s -> SpanWithLabel s (LexicalFastString $ occNameFS (getOccName bndr)))
                  <$> srcSpanToRealSrcSpan (nameSrcSpan (getName bndr))
  recordStgIdPosition bndr best_span bndr_span

collectExpr :: StgExpr -> M StgExpr
collectExpr = go
  where
    go (StgApp occ as) = return $ StgApp occ as
    go (StgLit lit) = return $ StgLit lit
    go (StgConApp dc _mn as tys) = do
      n' <- numberDataCon dc []
      return (StgConApp dc n' as tys)
    go (StgOpApp op as ty) = return (StgOpApp op as ty)
    go (StgCase scrut bndr ty alts) =
      StgCase <$> collectExpr scrut <*> pure bndr <*> pure ty <*> mapM collectAlt alts
    go (StgLet ext bind body) = do
        bind' <- collectStgBind bind
        body' <- go body
        return (StgLet ext bind' body')
    go (StgLetNoEscape ext bind body) = do
        bind' <- collectStgBind bind
        body' <- go body
        return (StgLetNoEscape ext bind' body')

    go (StgTick tick e) = do
       let k = case tick of
                SourceNote ss fp -> withSpan (ss, fp)
                _ -> id
       e' <- k (go e)
       return (StgTick tick e')

collectAlt :: StgAlt -> M StgAlt
collectAlt alt = do e' <- collectExpr $ alt_rhs alt
                    return $! alt { alt_rhs = e' }

-- | Try to find the best source position surrounding a 'StgExpr'. The
-- heuristic strips ticks from the current expression until it finds one which
-- is from the module currently being compiled. This is the same method that
-- the DWARF information uses to give locations to info tables.
--
-- It is usually a better alternative than using the 'RealSrcSpan' which is carefully
-- propagated downwards by 'withSpan'. It's "quick" because it works only using immediate context rather
-- than looking at the parent context like 'withSpan'
quickSourcePos :: FastString -> StgExpr -> Maybe SpanWithLabel
quickSourcePos cur_mod (StgTick (SourceNote ss m) e)
  | srcSpanFile ss == cur_mod = Just (SpanWithLabel ss m)
  | otherwise = quickSourcePos cur_mod e
quickSourcePos _ _ = Nothing

recordStgIdPosition :: Id -> Maybe SpanWithLabel -> Maybe SpanWithLabel -> M ()
recordStgIdPosition id best_span ss = do
  opts <- asks rOpts
  when (stgDebug_infoTableMap opts) $ do
    cc <- asks rSpan
    --Useful for debugging why a certain Id gets given a certain span
    --pprTraceM "recordStgIdPosition" (ppr id $$ ppr cc $$ ppr best_span $$ ppr ss)
    let mbspan = (\(SpanWithLabel rss d) -> (rss, d)) <$> (best_span <|> cc <|> ss)
    lift $ modify (\env -> env { provClosure = addToUniqMap (provClosure env) (idName id) (idType id, mbspan) })

-- | If -fdistinct-contructor-tables is enabled, each occurrance of a data
-- constructor will be given its own info table
numberDataCon :: DataCon -> [StgTickish] -> M ConstructorNumber
-- Unboxed tuples and sums do not allocate so they
-- have no info tables.
numberDataCon dc _ | isUnboxedTupleDataCon dc = return NoNumber
numberDataCon dc _ | isUnboxedSumDataCon dc = return NoNumber
numberDataCon dc ts = do
  opts <- asks rOpts
  if shouldMakeDistinctTable opts dc then do
    -- -fdistinct-constructor-tables is enabled and we do want to make distinct
    -- tables for this constructor. Add an entry to the data constructor map for
    -- this occurence of the data constructor with a unique number and a src
    -- span
    env <- lift get
    mcc <- asks rSpan
    let
      -- Guess a src span for this occurence using source note ticks and the
      -- current span in the environment
      !mbest_span = selectTick ts <|> (\(SpanWithLabel rss l) -> (rss, l)) <$> mcc

      -- Add the occurence to the data constructor map of the InfoTableProvMap,
      -- noting the unique number assigned for this occurence
      (!r, !dcMap') =
        alterUniqMap_L
          ( maybe
              (Just ((0, mbest_span) :| [] ))
              ( \xs@((k, _):|_) ->
                  Just $! ((k + 1, mbest_span) `NE.cons` xs)
              )
          )
          (provDC env)
          dc
    lift $ put (env { provDC = dcMap' })
    return $ case r of
      Nothing -> NoNumber
      Just res -> Numbered (fst (NE.head res))
  else do
    -- -fdistinct-constructor-tables is not enabled, or we do not want to make
    -- distinct tables for this specific constructor
    return NoNumber

selectTick :: [StgTickish] -> Maybe (RealSrcSpan, LexicalFastString)
selectTick = foldl' go Nothing
  where
    go :: Maybe (RealSrcSpan, LexicalFastString) -> StgTickish -> Maybe (RealSrcSpan, LexicalFastString)
    go _   (SourceNote rss d) = Just (rss, d)
    go acc _                  = acc

-- | Parse a string of comma-separated constructor names into a 'Set' of
-- 'String's with one entry per constructor.
parseDistinctConstructorTablesArg :: String -> Set String
parseDistinctConstructorTablesArg =
      -- Ensure we insert the last constructor name built by the fold, if not
      -- empty
      uncurry insertNonEmpty
    . foldr go ("", Set.empty)
  where
    go :: Char -> (String, Set String) -> (String, Set String)
    go ',' (cur, acc) = ("", Set.insert cur acc)
    go c   (cur, acc) = (c : cur, acc)

    insertNonEmpty :: String -> Set String -> Set String
    insertNonEmpty "" = id
    insertNonEmpty cs = Set.insert cs

-- | Descide whether a distinct info table should be made for a usage of a data
-- constructor. We only want to do this if -fdistinct-constructor-tables was
-- given and this constructor name was given, or no constructor names were
-- given.
shouldMakeDistinctTable :: StgDebugOpts -> DataCon -> Bool
shouldMakeDistinctTable StgDebugOpts{..} dc =
    case stgDebug_distinctConstructorTables of
      All -> True
      Only these -> Set.member dcStr these
      AllExcept these -> Set.notMember dcStr these
      None -> False
  where
    dcStr = occNameString . occName $ dataConName dc

{-
Note [Mapping Info Tables to Source Positions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This note describes what the `-finfo-table-map` flag achieves.

When debugging memory issues it is very useful to be able to map a specific closure
to a position in the source. The prime example is being able to map a THUNK to
a specific place in the source program, the mapping is usually quite precise because
a fresh info table is created for each distinct THUNK.

The info table map is also used to generate stacktraces.
See Note [Stacktraces from Info Table Provenance Entries (IPE based stack unwinding)]
for details.

There are three parts to the implementation

1. In GHC.Stg.Debug, the SourceNote information is used in order to give a source location
   to some specific closures.
2. In GHC.Driver.GenerateCgIPEStub, the actually used info tables are collected after the
   Cmm pipeline. This is important as it's hard to predict beforehand what code generation
   will do and which ids will end up in the generated program. Additionally, info tables of
   return frames (used to create stacktraces) are generated in the Cmm pipeline and aren't
   available before.
3. During code generation, a mapping from the info table to the statically determined location
   is emitted which can then be queried at runtime by various tools.

-- Giving Source Locations to Closures

At the moment thunk and constructor closures are added to the map. This information
is collected in the `InfoTableProvMap` which provides a mapping from:

1. Data constructors to a list of where they are used.
2. `Name`s and where they originate from.
3. Stack represented info tables (return frames) to an approximated source location
   of the call that pushed a continuation on the stacks.

During the CoreToStg phase, this map is populated whenever something is turned into
a StgRhsClosure or an StgConApp. The current source position is recorded
depending on the location indicated by the surrounding SourceNote.

The functions which add information to the map are `recordStgIdPosition` and
`numberDataCon`.

When the `-fdistinct-constructor-tables` flag is turned on then every
usage of a data constructor gets its own distinct info table. This is orchestrated
in `collectExpr` where an incrementing number is used to distinguish each
occurrence of a data constructor.

-- GenerateCgIPEStub

The info tables which are actually used in the generated program are collected after
the Cmm pipeline. `initInfoTableProv` is used to create a CStub, that initializes the
map in C code.

This step has to be done after the Cmm pipeline to make sure that all info tables are
really used and, even more importantly, return frame info tables are generated by the
pipeline.

-- Code Generation

The output of these two phases is combined together during code generation.
A C stub is generated which creates the static map from info table pointer to the
information about where that info table was created from. This is created by
`ipInitCode` in the same manner as a C stub is generated for cost centres.

This information can be consumed in two ways.

1. The complete mapping is emitted into the eventlog so that external tools such
as eventlog2html can use the information with the heap profile by info table mode.
2. The `lookupIPE` function can be used via the `whereFrom#` primop to introspect
information about a closure in a running Haskell program.

Note [Distinct Info Tables for Constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the old times, each usage of a data constructor used the same info table.
This made it impossible to distinguish which actual usage of a data constructor was
contributing primarily to the allocation in a program. Using the `-fdistinct-constructor-tables` flag you
can cause code generation to generate a distinct info table for each usage of
a constructor. Then, when inspecting the heap you can see precisely which usage of a constructor
was responsible for each allocation.

-}
