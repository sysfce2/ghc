{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Cmm.Dominators
  (
  -- * Dominator analysis and representation of results
    DominatorSet(..)
  , GraphWithDominators(..)
  , RPNum
  , graphWithDominators

  -- * Utility functions on graphs or graphs-with-dominators
  , graphMap
  , gwdRPNumber
  , gwdDominatorsOf
  , gwdDominatorTree

  -- * Utility functions on dominator sets
  , dominatorsMember
  , intersectDominators
  )
where

import GHC.Prelude

import Data.Array.IArray
import Data.Foldable()
import qualified Data.Tree as Tree

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import qualified GHC.CmmToAsm.CFG.Dominators as LT

import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm

import GHC.Utils.Outputable(Outputable(..), text, int, hcat, (<+>))
import GHC.Utils.Panic


-- | =Dominator sets
--
-- Node X dominates node Y if and only if every path from the entry to
-- Y includes X.  Node Y technically dominates itself, but it is
-- never included in the *representation* of its dominator set.
--
-- A dominator set is represented as a linked list in which each node
-- points to its *immediate* dominator, which is its parent in the
-- dominator tree.  In many circumstances the immediate dominator
-- will be the only dominator of interest.

data DominatorSet = ImmediateDominator { ds_label  :: Label
                                       , ds_parent :: DominatorSet
                                       }
                  | EntryNode
  deriving (Eq)

instance Outputable DominatorSet where
  ppr EntryNode = text "entry"
  ppr (ImmediateDominator l parent) = ppr l <+> text "->" <+> ppr parent



-- | Reverse postorder number of a node in a CFG
newtype RPNum = RPNum Int
  deriving (Eq, Ord)
-- in reverse postorder, nodes closer to the entry have smaller numbers

instance Show RPNum where
  show (RPNum i) = "RP" ++ show i

instance Outputable RPNum where
  ppr (RPNum i) = hcat [text "RP", int i]
   -- using `(<>)` would conflict with Semigroup



dominatorsMember :: Label -> DominatorSet -> Bool
-- ^ Use to tell if the given label is in the given
-- dominator set.  Which is to say, does the bloc
-- with with given label _properly_ and _non-vacuously_
-- dominate the node whose dominator set this is?
--
-- Takes linear time in the height of the dominator tree,
-- but uses space efficiently.
dominatorsMember lbl (ImmediateDominator l p) = l == lbl || dominatorsMember lbl p
dominatorsMember _   EntryNode = False


-- | Intersect two dominator sets to produce a third dominator set.
-- This function takes time linear in the size of the sets.
-- As such it is inefficient and should be used only for things
-- like visualizations or linters.
intersectDominators :: DominatorSet -> DominatorSet -> DominatorSet
intersectDominators ds ds' = commonPrefix (revDoms ds []) (revDoms ds' []) EntryNode
  where revDoms EntryNode prev = prev
        revDoms (ImmediateDominator lbl doms) prev = revDoms doms (lbl:prev)
        commonPrefix (a:as) (b:bs) doms
            | a == b = commonPrefix as bs (ImmediateDominator a doms)
        commonPrefix _ _ doms = doms


-- | The result of dominator analysis.  Also includes a reverse
-- postorder numbering, which is needed for dominator analysis
-- and for other (downstream) analyses.
--
-- Invariant: Dominators, graph, and RP numberings include only *reachable* blocks.
data GraphWithDominators node =
    GraphWithDominators { gwd_graph :: GenCmmGraph node
                        , gwd_dominators :: LabelMap DominatorSet
                        , gwd_rpnumbering :: LabelMap RPNum
                        }


-- | Call this function with a `CmmGraph` to get back the results of a
-- dominator analysis of that graph (as well as a reverse postorder
-- numbering).  The result also includes the subgraph of the original
-- graph that contains only the reachable blocks.
graphWithDominators :: forall node .
       (NonLocal node)
       => GenCmmGraph node
       -> GraphWithDominators node

-- The implementation uses the Lengauer-Tarjan algorithm from the x86
-- back end.

graphWithDominators g = GraphWithDominators (reachable rpblocks g) dmap rpmap
      where rpblocks = revPostorderFrom (graphMap g) (g_entry g)
            rplabels' = map entryLabel rpblocks
            rplabels :: Array Int Label
            rplabels = listArray bounds rplabels'

            rpmap :: LabelMap RPNum
            rpmap = mapFromList $ zipWith kvpair rpblocks [0..]
              where kvpair block i = (entryLabel block, RPNum i)

            labelIndex :: Label -> Int
            labelIndex = flip (mapFindWithDefault undefined) imap
              where imap :: LabelMap Int
                    imap = mapFromList $ zip rplabels' [0..]
            blockIndex = labelIndex . entryLabel

            bounds = (0, length rpblocks - 1)

            ltGraph :: [Block node C C] -> LT.Graph
            ltGraph [] = IM.empty
            ltGraph (block:blocks) =
                IM.insert
                      (blockIndex block)
                      (IS.fromList $ map labelIndex $ successors block)
                      (ltGraph blocks)

            idom_array :: Array Int LT.Node
            idom_array = array bounds $ LT.idom (0, ltGraph rpblocks)

            domSet 0 = EntryNode
            domSet i = ImmediateDominator (rplabels ! d) (doms ! d)
                where d = idom_array ! i
            doms = tabulate bounds domSet

            dmap = mapFromList $ zipWith (\lbl i -> (lbl, domSet i)) rplabels' [0..]

reachable :: NonLocal node => [Block node C C] -> GenCmmGraph node -> GenCmmGraph node
reachable blocks g = g { g_graph = GMany NothingO blockmap NothingO }
  where blockmap = mapFromList [(entryLabel b, b) | b <- blocks]


-- | =Utility functions

-- | Call `graphMap` to get the mapping from `Label` to `Block` that
-- is embedded in every `CmmGraph`.
graphMap :: GenCmmGraph n -> LabelMap (Block n C C)
graphMap (CmmGraph { g_graph = GMany NothingO blockmap NothingO }) = blockmap

-- | Use `gwdRPNumber` on the result of the dominator analysis to get
-- a mapping from the `Label` of each reachable block to the reverse
-- postorder number of that block.
gwdRPNumber :: GraphWithDominators node -> Label -> RPNum
gwdRPNumber g l = mapFindWithDefault unreachable l (gwd_rpnumbering g)

-- | Use `gwdDominatorsOf` on the result of the dominator analysis to get
-- a mapping from the `Label` of each reachable block to the dominator
-- set (and the immediate dominator) of that block.  The
-- implementation is space-efficient: intersecting dominator
-- sets share the representation of their intersection.

gwdDominatorsOf :: GraphWithDominators node -> Label -> DominatorSet
gwdDominatorsOf g lbl = mapFindWithDefault unreachable lbl (gwd_dominators g)

gwdDominatorTree :: GraphWithDominators node -> Tree.Tree Label
gwdDominatorTree g = subtreeAt (g_entry (gwd_graph gwd))
  where subtreeAt label = Tree.Node label $ map subtreeAt $ children label
        children l = mapFindWithDefault [] l child_map
        child_map = mapFoldlWithKey addParent mapEmpty $ gwd_dominators g
          where addParent cm _ EntryNode = cm
                addParent cm lbl (ImmediateDominator p _) =
                    mapInsertWith (++) p [lbl] cm


-- | Turn a function into an array.  Inspired by SML's `Array.tabulate`
tabulate :: (Ix i) => (i, i) -> (i -> e) -> Array i e
tabulate b f = listArray b $ map f $ range b

unreachable :: a
unreachable = panic "unreachable node in GraphWithDominators"
