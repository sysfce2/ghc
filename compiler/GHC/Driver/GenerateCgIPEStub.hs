{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

module GHC.Driver.GenerateCgIPEStub (generateCgIPEStub) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Semigroup ((<>))
import GHC.Cmm
import GHC.Cmm.CLabel (CLabel)
import GHC.Cmm.Dataflow (Block, C, O)
import GHC.Cmm.Dataflow.Block (blockSplit, blockToList)
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label (Label, LabelMap)
import GHC.Cmm.Info.Build (emptySRT)
import GHC.Cmm.Pipeline (cmmPipeline)
import GHC.Data.Maybe (firstJusts)
import GHC.Data.Stream (Stream, liftIO)
import qualified GHC.Data.Stream as Stream
import GHC.Driver.Env (hsc_dflags, hsc_logger)
import GHC.Driver.Env.Types (HscEnv)
import GHC.Driver.Flags (GeneralFlag (..), DumpFlag(Opt_D_ipe_stats))
import GHC.Driver.DynFlags (gopt, targetPlatform)
import GHC.Driver.Config.StgToCmm
import GHC.Driver.Config.Cmm
import GHC.Prelude
import GHC.Runtime.Heap.Layout (isStackRep)
import GHC.Settings (Platform, platformTablesNextToCode)
import GHC.StgToCmm.Monad (getCmm, initC, runC, initFCodeState)
import GHC.StgToCmm.Prof (initInfoTableProv)
import GHC.StgToCmm.Types (CmmCgInfos (..), ModuleLFInfos)
import GHC.StgToCmm.Utils
import GHC.Types.IPE (InfoTableProvMap (provInfoTables), IpeSourceLocation)
import GHC.Types.Name.Set (NonCaffySet)
import GHC.Types.Tickish (GenTickish (SourceNote))
import GHC.Unit.Types (Module, moduleName)
import GHC.Unit.Module (moduleNameString)
import qualified GHC.Utils.Logger as Logger
import GHC.Utils.Outputable (ppr)

{-
Note [Stacktraces from Info Table Provenance Entries (IPE based stack unwinding)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Stacktraces can be created from return frames as they are pushed to stack for every case scrutinee.
But to make them readable / meaningful, one needs to know the source location of each return frame.

Every return frame has a distinct info table and thus a distinct code pointer (for tables next to
code) or at least a distinct address itself. Info Table Provenance Entries (IPEs) are searchable by
this pointer and contain a source location.

The info table / info table code pointer to source location map is described in:
Note [Mapping Info Tables to Source Positions]

To be able to lookup IPEs for return frames one needs to emit them during compile time. This is done
by `generateCgIPEStub`.

This leads to the question: How to figure out the source location of a return frame?

While the lookup algorithms when tables-next-to-code is on/off differ in details, they have in
common that we want to lookup the `CmmNode.CmmTick` (containing a `SourceNote`) that is nearest
(before) the usage of the return frame's label. (Which label and label type is used differs between
these two use cases.)

With tables-next-to-code
~~~~~~~~~~~~~~~~~~~~~~~~

Let's consider this example:
```
 Main.returnFrame_entry() { //  [R2]
         { info_tbls: [(c18g,
                        label: block_c18g_info
                        rep: StackRep []
                        srt: Just GHC.CString.unpackCString#_closure),
                       (c18r,
                        label: Main.returnFrame_info
                        rep: HeapRep static { Fun {arity: 1 fun_type: ArgSpec 5} }
                        srt: Nothing)]
           stack_info: arg_space: 8
         }
     {offset

      [...]

       c18u: // global
           //tick src<Main.hs:(7,1)-(16,15)>
           I64[Hp - 16] = sat_s16B_info;
           P64[Hp] = _s16r::P64;
           _c17j::P64 = Hp - 16;
           //tick src<Main.hs:8:25-39>
           I64[Sp - 8] = c18g;
           R3 = _c17j::P64;
           R2 = GHC.IO.Unsafe.unsafePerformIO_closure;
           R1 = GHC.Base.$_closure;
           Sp = Sp - 8;
           call stg_ap_pp_fast(R3,
                               R2,
                               R1) returns to c18g, args: 8, res: 8, upd: 8;
```

The return frame `block_c18g_info` has the label `c18g` which is used in the call to `stg_ap_pp_fast`
(`returns to c18g`) as continuation (`cml_cont`). The source location we're after, is the nearest
`//tick` before the call (`//tick src<Main.hs:8:25-39>`).

In code the Cmm program is represented as a Hoopl graph. Hoopl distinguishes nodes by defining if they
are open or closed on entry (one can fallthrough to them from the previous instruction) and if they are
open or closed on exit (one can fallthrough from them to the next node).

Please refer to the paper "Hoopl: A Modular, Reusable Library for Dataflow Analysis and Transformation"
for a detailed explanation.

Here we use the fact, that calls (represented by `CmmNode.CmmCall`) are always closed on exit
(`CmmNode O C`, `O` means open, `C` closed). In other words, they are always at the end of a block.

So, given a stack represented info table (likely representing a return frame, but this isn't completely
sure as there are e.g. update frames, too) with it's label (`c18g` in the example above) and a `CmmGraph`:
  - Look at the end of every block, if it's a `CmmNode.CmmCall` returning to the continuation with the
    label of the return frame.
  - If there's such a call, lookup the nearest `CmmNode.CmmTick` by traversing the middle part of the block
    backwards (from end to beginning).
  - Take the first `CmmNode.CmmTick` that contains a `Tickish.SourceNote` and return it's payload as
    `IpeSourceLocation`. (There are other `Tickish` constructors like `ProfNote` or `HpcTick`, these are
    ignored.)

Without tables-next-to-code
~~~~~~~~~~~~~~~~~~~~~~~~~~~

When tables-next-to-code is off, there is no return frame / continuation label in calls. The continuation (i.e. return
frame) is set in an explicit Cmm assignment. Thus the tick lookup algorithm has to be slightly different.

```
 sat_s16G_entry() { //  [R1]
         { info_tbls: [(c18O,
                        label: sat_s16G_info
                        rep: HeapRep { Thunk }
                        srt: Just _u18Z_srt)]
           stack_info: arg_space: 0
         }
     {offset
       c18O: // global
           _s16G::P64 = R1;
           if ((Sp + 8) - 40 < SpLim) (likely: False) goto c18P; else goto c18Q;
       c18P: // global
           R1 = _s16G::P64;
           call (stg_gc_enter_1)(R1) args: 8, res: 0, upd: 8;
       c18Q: // global
           I64[Sp - 16] = stg_upd_frame_info;
           P64[Sp - 8] = _s16G::P64;
           //tick src<Main.hs:20:9-13>
           I64[Sp - 24] = block_c18M_info;
           R1 = GHC.Show.$fShow[]_closure;
           P64[Sp - 32] = GHC.Show.$fShowChar_closure;
           Sp = Sp - 32;
           call stg_ap_p_fast(R1) args: 16, res: 8, upd: 24;
     }
 },
 _blk_c18M() { //  [R1]
         { info_tbls: [(c18M,
                        label: block_c18M_info
                        rep: StackRep []
                        srt: Just System.IO.print_closure)]
           stack_info: arg_space: 0
         }
     {offset
       c18M: // global
           _s16F::P64 = R1;
           R1 = System.IO.print_closure;
           P64[Sp] = _s16F::P64;
           call stg_ap_p_fast(R1) args: 32, res: 0, upd: 24;
     }
 },
```

In this example we have to lookup `//tick src<Main.hs:20:9-13>` for the return frame `c18M`.
Notice, that this cannot be done with the `Label` `c18M`, but with the `CLabel` `block_c18M_info`
(`label: block_c18M_info` is actually a `CLabel`).

The find the tick:
  - Every `Block` is checked from top (first) to bottom (last) node for an assignment like
   `I64[Sp - 24] = block_c18M_info;`. The lefthand side is actually ignored.
  - If such an assignment is found the search is over, because the payload (content of
    `Tickish.SourceNote`, represented as `IpeSourceLocation`) of last visited tick is always
    remembered in a `Maybe`.
-}

generateCgIPEStub :: HscEnv -> Module -> InfoTableProvMap -> Stream IO CmmGroupSRTs (NonCaffySet, ModuleLFInfos) -> Stream IO CmmGroupSRTs CmmCgInfos
generateCgIPEStub hsc_env this_mod denv s = do
  let dflags   = hsc_dflags hsc_env
      platform = targetPlatform dflags
      logger   = hsc_logger hsc_env
      fstate   = initFCodeState platform
      cmm_cfg  = initCmmConfig dflags
  cgState <- liftIO initC

  -- Collect info tables from the Cmm if -finfo-table-map is enabled. If
  -- -finfo-table-map is not enabled, infoTablesWithTickishes will be empty. If
  -- -finfo-table-map-with-stack is enabled, any STACK info tables will be
  -- mapped to their source locations (See Note [Stacktraces from Info Table
  -- Provenance Entries (IPE based stack unwinding)]). If
  -- -finfo-table-map-with-stack is not enabled, we need to track how many STACK
  -- info tables we have skipped (in case -dipe-stats is enabled). Note that
  -- this is the only stats tracking we do at this stage, so initStats here
  -- should only ever contain stats about skipped STACK info tables.
  let
    collectFun =
      if gopt Opt_InfoTableMap dflags then
        collect platform
      else
        collectNothing platform

  ((infoTablesWithTickishes, initStats), (nonCaffySet, moduleLFInfos)) <- Stream.mapAccumL_ collectFun (mempty, mempty) s

  -- Yield Cmm for Info Table Provenance Entries (IPEs)
  let denv' = denv {provInfoTables = Map.mapKeys cit_lbl infoTablesWithTickishes}
      ((mIpeStub, ipeCmmGroup), _) = runC (initStgToCmmConfig dflags this_mod) fstate cgState $ getCmm (initInfoTableProv initStats (Map.keys infoTablesWithTickishes) denv')

  (_, ipeCmmGroupSRTs) <- liftIO $ cmmPipeline logger cmm_cfg (emptySRT this_mod) ipeCmmGroup
  Stream.yield ipeCmmGroupSRTs

  ipeStub <-
    case mIpeStub of
      Just (stats, stub) -> do
        -- Print ipe stats if requested
        liftIO $
          Logger.putDumpFileMaybe logger
            Opt_D_ipe_stats
            ("IPE Stats for module " ++ (moduleNameString $ moduleName this_mod))
            Logger.FormatText
            (ppr stats)
        return stub
      Nothing -> return mempty

  return CmmCgInfos {cgNonCafs = nonCaffySet, cgLFInfos = moduleLFInfos, cgIPEStub = ipeStub}
  where
    -- These functions are applied to the elements of the stream of
    -- CmmGroupSRTs. 'collect' populates a map from info table to potential
    -- source location, and is used when -finfo-table-map is supplied.
    -- 'collectNothing' does nothing and just throws out the stream elements.
    collect, collectNothing
      :: Platform
      -> (Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats)
      -> CmmGroupSRTs
      -> IO ((Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats), CmmGroupSRTs)
    collect platform (!acc, !stats) cmmGroupSRTs = do
      let
        blocks = concatMap toBlockList (graphs cmmGroupSRTs)
        labelsToInfoTables = collectInfoTables cmmGroupSRTs
        (tablesToTickishes, stats') = mapFoldlWithKey (lookupEstimatedTick platform blocks) (acc, stats) labelsToInfoTables
      return ((tablesToTickishes, stats'), cmmGroupSRTs)
    collectNothing _ _ cmmGroupSRTs = pure ((Map.empty, mempty), cmmGroupSRTs)

    collectInfoTables :: CmmGroupSRTs -> LabelMap CmmInfoTable
    collectInfoTables cmmGroup = foldl' extractInfoTables mapEmpty cmmGroup

    extractInfoTables :: LabelMap CmmInfoTable -> GenCmmDecl RawCmmStatics CmmTopInfo CmmGraph -> LabelMap CmmInfoTable
    extractInfoTables acc (CmmProc h _ _ _) = acc `mapUnion` info_tbls h
    extractInfoTables acc _ = acc

    lookupEstimatedTick
      :: Platform
      -> [CmmBlock]
      -> (Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats)
      -> Label
      -> CmmInfoTable
      -> (Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats)
    lookupEstimatedTick platform blocks (!acc, !stats) infoTableLabel infoTable = do
      -- All return frame info tables are stack represented, though not all stack represented info
      -- tables have to be return frames.
      if (isStackRep . cit_rep) infoTable then
        if gopt Opt_InfoTableMapWithStack (hsc_dflags hsc_env) then
          -- This is a STACK info table and we DO want to put it in the info
          -- table map
          let
            findFun =
              if platformTablesNextToCode platform
                then findCmmTickishWithTNTC infoTableLabel
                else findCmmTickishSansTNTC (cit_lbl infoTable)
            -- Avoid retaining the blocks
            !srcloc =
              case firstJusts $ map findFun blocks of
                Just !srcloc -> Just srcloc
                Nothing -> Nothing
          in
            (Map.insert infoTable srcloc acc, stats)
        else
          -- This is a STACK info table but we DO NOT want to put it in the info
          -- table map (-fno-info-table-map-with-stack was given), track it as
          -- skipped
            (acc, stats <> skippedIpeStats)

      else
        -- This is not a STACK info table, so put it in the map with no source
        -- location (for now)
        (Map.insert infoTable Nothing acc, stats)

    graphs :: CmmGroupSRTs -> [CmmGraph]
    graphs = foldl' go []
      where
        go :: [CmmGraph] -> GenCmmDecl d h CmmGraph -> [CmmGraph]
        go acc (CmmProc _ _ _ g) = g : acc
        go acc _ = acc

    findCmmTickishWithTNTC :: Label -> Block CmmNode C C -> Maybe IpeSourceLocation
    findCmmTickishWithTNTC label block = do
      let (_, middleBlock, endBlock) = blockSplit block

      isCallWithReturnFrameLabel endBlock label
      lastTickInBlock middleBlock
      where
        isCallWithReturnFrameLabel :: CmmNode O C -> Label -> Maybe ()
        isCallWithReturnFrameLabel (CmmCall _ (Just l) _ _ _ _) clabel | l == clabel = Just ()
        isCallWithReturnFrameLabel _ _ = Nothing

        lastTickInBlock block =
          listToMaybe $
              mapMaybe maybeTick $ (reverse . blockToList) block

        maybeTick :: CmmNode O O -> Maybe IpeSourceLocation
        maybeTick (CmmTick (SourceNote span name)) = Just (span, name)
        maybeTick _ = Nothing

    findCmmTickishSansTNTC :: CLabel -> Block CmmNode C C -> Maybe IpeSourceLocation
    findCmmTickishSansTNTC cLabel block = do
      let (_, middleBlock, _) = blockSplit block
      find cLabel (blockToList middleBlock) Nothing
      where
        find :: CLabel -> [CmmNode O O] -> Maybe IpeSourceLocation -> Maybe IpeSourceLocation
        find label (b : blocks) lastTick = case b of
          (CmmStore _ (CmmLit (CmmLabel l)) _) -> if label == l then lastTick else find label blocks lastTick
          (CmmTick (SourceNote span name)) -> find label blocks $ Just (span, name)
          _ -> find label blocks lastTick
        find _ [] _ = Nothing
