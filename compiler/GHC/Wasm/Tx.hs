{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators, KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, StandaloneKindSignatures, PolyKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}

module GHC.Wasm.Tx
  ( tx
  , CG(..)
  , WasmExpr

  , WasmExprs
  , txs

  , call
  )

where

import GHC.Prelude

import Data.Type.Equality

import qualified GHC.Cmm.Type as CT
import GHC.Cmm.Expr
import GHC.Cmm.Node
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain (assert)
import GHC.Wasm.IR

import GHC.Cmm.Dataflow.Block

----------------------------------------------------------------
---
---     Overview
---
----------------------------------------------------------------
--
-- This module translates Cmm expressions to Wasm instructions while
-- meeting the following goals:
--   * Keep the code compact and readable
--   * Use GADTs to track the stack types of the Wasm code
--   * Check Cmm types only when necessary to prove type correctness
--     of the generated code or when debugging GHC
--



----------------------------------------------------------------
-- code-generation monad

-- This class is a placeholder that expresses the only
-- property used in the prototype: the platform Boolean
-- type is discoverable.

class Monad (codegen bool) => CG bool codegen where
  booleanWasmTypeTag :: codegen bool (WasmTypeTag bool)

----------------------------------------------------------------

-- Each Cmm expression is translated into a "WebAssembly expression of
-- type t."  This is Wasm code that can push a value of type `t` onto
-- *any* evaluation stack.

type WasmExpr bool t = (forall stack . WasmIR bool stack (t : stack))


-- At translation time, the target type `t` is not known.
-- It can be defined by a tag, but we wish to avoid the bookkeeping
-- associated with packing a tag and a translation into existentially
-- quantified pair.  Instead, the translator uses continuation-passing
-- style (CPS) to pass a tag and a translation to its continuation.
-- This technique is recommended by Richard Eisenberg, Stitch: The
-- Sound Type-Indexed Type Checker (Functional Pearl), Haskell
-- Symposium 2020 (https://doi.org/10.1145/3406088.3409015).

tx :: CG bool codegen
       => CmmExpr
       -> (forall t . WasmTypeTag t -> WasmExpr bool t -> codegen bool a)
       -> codegen bool a
  -- type `a` is the answer type of the continuation

-- The translation is organized as follows:
--
--   * The main translation function `tx` dispatches on the form of a
--     Cmm expression.
--
--   * For every different type of Cmm operator, `tx` calls a
--     different auxiliary function: `wasmUnary`, `wasmBinary`,
--     `wasmCompare`, and so on.
--
--     (Since every different type of Cmm operator generates
--     intermediate Wasm code of different types, it seems sensible
--     that each type of operator might require a Haskell translation
--     function of a different type.  But it's a bit irksome.)
--
--   * Each auxiliary function calls back into `tx` to translate
--     operands, if any, then composes the resulting code.
--
--  All functions are CPS.

tx expr k =
  case expr of
    CmmLit (CmmInt n w)   -> wasmNullaryInt   w (flip WasmInt   n) k
    CmmLit (CmmFloat x w) -> wasmNullaryFloat w (flip WasmFloat x) k

    CmmMachOp (MO_Not w) es -> wasmUnary w es WasmNot k

    CmmMachOp (MO_Add w) es -> wasmBinary w es WasmAdd k
    CmmMachOp (MO_Sub w) es -> wasmBinary w es WasmSub k

    CmmMachOp (MO_S_Ge w) es -> wasmCompare w es WasmS_Ge k


    _ -> panic "unimplemented"

------ Types of all the translation functions

-- | Cmm integer and floating-point literals (with zero operands)

wasmNullaryInt, wasmNullaryFloat ::
      CG bool codegen
   => CT.Width
   -> (forall t stack . WasmTypeTag t -> WasmIR bool (stack) (t : stack))
   -> (forall t . WasmTypeTag t -> WasmExpr bool t -> codegen bool r)
   -> codegen bool r


-- | Cmm unary operators of type `t -> t`

wasmUnary  :: CG bool codegen
           => CT.Width
           -> [CmmExpr]
           -> (forall t stack . WasmTypeTag t -> WasmIR bool (t : stack) (t : stack))
           -> (forall t . WasmTypeTag t -> WasmExpr bool t -> codegen bool r)
           -> codegen bool r


-- | Cmm binary operators of type `t -> t -> t`

wasmBinary ::
    CG bool codegen
 => CT.Width
 -> [CmmExpr]
 -> (forall t stack . WasmTypeTag t -> WasmIR bool (t : t : stack) (t : stack))
 -> (forall t . WasmTypeTag t -> WasmExpr bool t -> codegen bool r)
 -> codegen bool r


-- | Cmm binary operators of type `t -> t -> bool`

wasmCompare ::
      forall bool codegen r . CG bool codegen
   => CT.Width
   -> [CmmExpr]
   -> (forall t stack . WasmTypeTag t -> WasmIR bool (t : t : stack) (bool : stack))
   -> (WasmTypeTag bool -> WasmExpr bool bool -> codegen bool r)
   -> codegen bool r

---------- Implementations of the translation functions

wasmNullaryInt w operator k =
  withIntWidthTag w $ \tag -> k tag (operator tag)


wasmNullaryFloat w operator k =
  withFloatWidthTag w $ \tag -> k tag (operator tag)

wasmUnary w [e] operator k =
    tx e $ \tag code -> assert (tag `hasWidth` w) $ k tag (code <> operator tag)
wasmUnary _ _ _ _ = panic "wrong number of operands to unary operator in Cmm"

wasmBinary w es operator k =
    binaryCPS es $ \tag code1 code2 ->
        assert (tag `hasWidth` w) $
        k tag (code1 <> code2 <> operator tag)

wasmCompare w es operator k =
    binaryCPS es $ \tag code1 code2 -> do
      bool <- booleanWasmTypeTag
      assert (bool `hasWidth` w) $
       k bool (code1 <> code2 <> operator tag)

binaryCPS
       :: forall bool codegen a . CG bool codegen
       => [CmmExpr]
       -> (forall t .  WasmTypeTag t
                    -> WasmExpr bool t
                    -> WasmExpr bool t
                    -> codegen bool a)
       -> codegen bool a

binaryCPS [e1, e2] k =   -- would dearly love to use do notation here
    tx e1 $ \tag1 code1 ->
    tx e2 $ \tag2 code2 ->
    case tag1 `testEquality` tag2 of -- mandatory check
      Just Refl -> k tag1 code1 code2
      Nothing -> panic "ill-typed Cmm"
binaryCPS _ _ = panic "wrong number of operands to binary operator in Cmm"

----------------------------------------------------------------


hasWidth :: WasmTypeTag t -> CT.Width -> Bool
hasWidth TagI32 CT.W32 = True
hasWidth TagF32 CT.W32 = True
hasWidth TagI64 CT.W64 = True
hasWidth TagF64 CT.W64 = True
hasWidth _ _ = False


withIntWidthTag :: CT.Width -> (forall t . WasmTypeTag t -> a) -> a
withIntWidthTag CT.W32 k = k TagI32
withIntWidthTag CT.W64 k = k TagI64
withIntWidthTag w _ = panic $ "width " ++ show w ++ " not supported on wasm target"

withFloatWidthTag :: CT.Width -> (forall t . WasmTypeTag t -> a) -> a
withFloatWidthTag CT.W32 k = k TagF32
withFloatWidthTag CT.W64 k = k TagF64
withFloatWidthTag w _ = panic $ "width " ++ show w ++ " not supported on wasm target"


----------------------------------------------------------------
-- new and experimental

type WasmExprs bool ts = (forall stack . WasmIR bool stack (RevAppend ts stack))

txs :: CG bool codegen
       => [CmmExpr]
       -> (forall ts . TypeList ts -> WasmExprs bool ts -> codegen bool a)
       -> codegen bool a
txs [] k = k TypeListNil WasmNop
txs (e:es) k = -- first expression is oldest on stack
  txs es $ \ts codes ->
    tx e $ \t code ->
      k (TypeListCons t ts) (code <> codes)

type WasmAction bool = (forall stack . WasmIR bool stack stack)

call :: CG bool codegen
     => CmmNode O O
     -> (WasmAction bool -> codegen bool a)
     -> codegen bool a
call (CmmUnsafeForeignCall _target [] arguments) k =
    -- ran out of time to deal with result registers
    txs arguments $ \ts codes ->
      k (codes <> WasmCallNoResults ts)
call _ _ = panic "more cases needed"
