{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.StgToJS.DataCon
  ( genCon
  , allocCon
  , allocUnboxedCon
  , allocDynamicE
  , allocDynamic
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make

import GHC.StgToJS.Closure
import GHC.StgToJS.ExprCtx
import GHC.StgToJS.Types
import GHC.StgToJS.Monad
import GHC.StgToJS.CoreUtils
import GHC.StgToJS.Profiling
import GHC.StgToJS.Utils
import GHC.StgToJS.Ids

import GHC.Core.DataCon

import GHC.Types.CostCentre
import GHC.Types.Unique.Map

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.FastString

import Data.Maybe

genCon :: ExprCtx -> DataCon -> [JExpr] -> G JStat
genCon ctx con args
  | isUnboxedTupleDataCon con
  = return $ assignToExprCtx ctx args

  | [ValExpr (JVar ctxi)] <- concatMap typex_expr (ctxTarget ctx)
  = allocCon ctxi con currentCCS args

  -- FIXME: (Sylvain 2022-03-11) Do we support e.g. "data T = MkT Word64"? It
  -- would return two JExprs

  | xs <- concatMap typex_expr (ctxTarget ctx)
  = pprPanic "genCon: unhandled DataCon" (ppr (con, args, xs))

allocCon :: Ident -> DataCon -> CostCentreStack -> [JExpr] -> G JStat
allocCon to con cc xs
  | isBoolDataCon con || isUnboxableCon con =
      return (toJExpr to |= allocUnboxedCon con xs)
{-  | null xs = do
      i <- varForId (dataConWorkId con)
      return (assignj to i) -}
  | otherwise = do
      e <- varForDataConWorker con
      cs <- getSettings
      prof <- profiling
      ccsJ <- if prof then ccsVarJ cc else return Nothing
      return $ allocDynamic cs False to e xs ccsJ

allocUnboxedCon :: DataCon -> [JExpr] -> JExpr
allocUnboxedCon con = \case
  []
    | isBoolDataCon con && dataConTag con == 1 -> false_
    | isBoolDataCon con && dataConTag con == 2 -> true_
  [x]
    | isUnboxableCon con -> x
  xs -> pprPanic "allocUnboxedCon: not an unboxed constructor" (ppr (con,xs))

allocDynamicE :: Bool          -- ^ csInlineAlloc from StgToJSConfig
              -> JExpr
              -> [JExpr]
              -> Maybe JExpr
              -> JExpr
allocDynamicE  inline_alloc entry free cc
  | inline_alloc || length free > 24 = newClosure $ Closure
      { clEntry  = entry
      , clField1 = fillObj1
      , clField2 = fillObj2
      , clMeta   = ValExpr (JInt 0)
      , clCC     = cc
      }
  | otherwise = ApplExpr allocFun (toJExpr entry : free ++ maybeToList cc)
  where
    allocFun = allocClsA (length free)
    (fillObj1,fillObj2)
       = case free of
                []  -> (null_, null_)
                [x] -> (x,null_)
                [x,y] -> (x,y)
                (x:xs) -> (x,toJExpr (JHash $ listToUniqMap (zip dataFields xs)))
    dataFields = map (mkFastString . ('d':) . show) [(1::Int)..]

allocDynamic :: StgToJSConfig -> Bool -> Ident -> JExpr -> [JExpr] -> Maybe JExpr -> JStat
allocDynamic s haveDecl to entry free cc =
  dec to `mappend` (toJExpr to |= allocDynamicE (csInlineAlloc s) entry free cc)
    where
      dec i | haveDecl  = DeclStat i
            | otherwise = mempty
