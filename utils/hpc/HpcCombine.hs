---------------------------------------------------------
-- The main program for the hpc-add tool, part of HPC.
-- Andy Gill, Oct 2006
---------------------------------------------------------

module HpcCombine (sum_plugin,combine_plugin,map_plugin) where

import Trace.Hpc.Tix
import Trace.Hpc.Util

import HpcFlags

import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IM
import qualified Data.Text as T

------------------------------------------------------------------------------
sum_options :: FlagOptSeq
sum_options
        = excludeOpt
        . includeOpt
        . outputOpt
        . unionModuleOpt
        . verbosityOpt

sum_plugin :: Plugin
sum_plugin = Plugin { name = "sum"
                    , usage = "[OPTION] .. <TIX_FILE> [<TIX_FILE> [<TIX_FILE> ..]]"
                    , options = sum_options
                    , summary = "Sum multiple .tix files in a single .tix file"
                    , implementation = sum_main
                    , init_flags = default_flags
                    , final_flags = default_final_flags
                    }

combine_options :: FlagOptSeq
combine_options
        = excludeOpt
        . includeOpt
        . outputOpt
        . combineFunOpt
        . combineFunOptInfo
        . unionModuleOpt
        . verbosityOpt

combine_plugin :: Plugin
combine_plugin = Plugin { name = "combine"
                        , usage = "[OPTION] .. <TIX_FILE> <TIX_FILE>"
                        , options = combine_options
                        , summary = "Combine two .tix files in a single .tix file"
                        , implementation = combine_main
                        , init_flags = default_flags
                        , final_flags = default_final_flags
                        }

map_options :: FlagOptSeq
map_options
        = excludeOpt
        . includeOpt
        . outputOpt
        . mapFunOpt
        . mapFunOptInfo
        . unionModuleOpt
        . verbosityOpt

map_plugin :: Plugin
map_plugin = Plugin { name = "map"
                    , usage = "[OPTION] .. <TIX_FILE> "
                    , options = map_options
                    , summary = "Map a function over a single .tix file"
                    , implementation = map_main
                    , init_flags = default_flags
                    , final_flags = default_final_flags
                    }

------------------------------------------------------------------------------

sum_main :: Flags -> [String] -> IO ()
sum_main _     [] = hpcError sum_plugin $ "no .tix file specified"
sum_main flags (first_file:more_files) = do
  Just tix <- readTix first_file

  tix' <- foldM (mergeTixFile flags (+))
                (filterTix flags tix)
                more_files

  case outputFile flags of
    "-" -> putStrLn (show tix')
    out -> writeTix out tix'

combine_main :: Flags -> [String] -> IO ()
combine_main flags [first_file,second_file] = do
  let f = theCombineFun (combineFun flags)

  Just tix1 <- readTix first_file
  Just tix2 <- readTix second_file

  let tix = mergeTix (mergeModule flags)
                     f
                     (filterTix flags tix1)
                     (filterTix flags tix2)

  case outputFile flags of
    "-" -> putStrLn (show tix)
    out -> writeTix out tix
combine_main _     _ = hpcError combine_plugin $ "need exactly two .tix files to combine"

map_main :: Flags -> [String] -> IO ()
map_main flags [first_file] = do
  let f = thePostFun (postFun flags)

  Just tix <- readTix first_file

  let (Tix inside_tix) = filterTix flags tix
  let tix' = Tix $ map (mapTixModule f) inside_tix

  case outputFile flags of
    "-" -> putStrLn (show tix')
    out -> writeTix out tix'
map_main _     [] = hpcError map_plugin $ "no .tix file specified"
map_main _     _  = hpcError map_plugin $ "to many .tix files specified"

mergeTixFile :: Flags -> (Integer -> Integer -> Integer) -> Tix -> String -> IO Tix
mergeTixFile flags fn tix file_name = do
  Just new_tix <- readTix file_name
  return $! strict $ mergeTix (mergeModule flags) fn tix (filterTix flags new_tix)

-- could allow different numbering on the module info,
-- as long as the total is the same; will require normalization.

mergeTix :: MergeFun
         -> (Integer -> Integer -> Integer) -> Tix -> Tix -> Tix
mergeTix modComb f
         (Tix t1)
         (Tix t2)  = Tix
         [ case (Map.lookup m fm1,Map.lookup m fm2) of
           -- todo, revisit the semantics of this combination
            (Just tm1, Just tm2)
               | tixModuleHash tm1 /= tixModuleHash tm2
               || tixModuleTickCount tm1 /= tixModuleTickCount tm2
                     -> error $ "mismatched in module " ++ T.unpack m
               | otherwise      ->
                     mkTixModule m (tixModuleHash tm1) (mergeTickCounts f (tixModuleTixs tm1) (tixModuleTixs tm2))
            (Just m1,Nothing) ->
                  m1
            (Nothing,Just m2) ->
                  m2
            _ -> error "impossible"
         | m <- Set.toList (theMergeFun modComb m1s m2s)
         ]
  where
   m1s = Set.fromList $ map tixModuleName t1
   m2s = Set.fromList $ map tixModuleName t2

   fm1 = Map.fromList [ (tixModuleName tix,tix)
                      | tix <- t1
                      ]
   fm2 = Map.fromList [ (tixModuleName tix,tix)
                      | tix <- t2
                      ]


-- What I would give for a hyperstrict :-)
-- This makes things about 100 times faster.
class Strict a where
   strict :: a -> a

instance Strict Integer where
   strict i = i

instance Strict Int where
   strict i = i

instance Strict Hash where      -- should be fine, because Hash is a newtype round an Int
   strict i = i

instance Strict Char where
   strict i = i

instance Strict a => Strict [a] where
   strict (a:as) = (((:) $! strict a) $! strict as)
   strict []     = []

instance (Strict a, Strict b) => Strict (a,b) where
   strict (a,b) = (((,) $! strict a) $! strict b)

instance Strict Tix where
  strict (Tix t1) =
            Tix $! strict t1

instance Strict TickCounts where
  strict i = i

instance Strict TixModule where
  strict (TixModule m1 p1 t1) =
            (((TixModule $! strict m1) $! strict p1)) $! strict t1

instance Strict T.Text where
  strict i = i
