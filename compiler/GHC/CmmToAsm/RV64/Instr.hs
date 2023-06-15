{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.CmmToAsm.RV64.Instr

where

import GHC.Prelude

import GHC.CmmToAsm.RV64.Cond
import GHC.CmmToAsm.RV64.Regs

import GHC.CmmToAsm.Instr (RegUsage(..))
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils
import GHC.CmmToAsm.Config
import GHC.Platform.Reg

import GHC.Platform.Regs
import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.Cmm
import GHC.Cmm.CLabel
import GHC.Utils.Outputable
import GHC.Platform
import GHC.Types.Unique.Supply

import GHC.Utils.Panic

import Data.Maybe (fromMaybe)

import GHC.Stack

-- | TODO: Should be `2 * spillSlotSize = 16`
stackFrameHeaderSize :: Platform -> Int
stackFrameHeaderSize _ = 64

-- | All registers are 8 byte wide.
spillSlotSize :: Int
spillSlotSize = 8

-- | The number of bytes that the stack pointer should be aligned
-- to.
stackAlign :: Int
stackAlign = 16

-- | The number of spill slots available without allocating more.
maxSpillSlots :: NCGConfig -> Int
maxSpillSlots config
--  = 0 -- set to zero, to see when allocMoreStack has to fire.
    = let platform = ncgPlatform config
      in ((ncgSpillPreallocSize config - stackFrameHeaderSize platform)
         `div` spillSlotSize) - 1

-- | Convert a spill slot number to a *byte* offset, with no sign.
spillSlotToOffset :: NCGConfig -> Int -> Int
spillSlotToOffset config slot
   = stackFrameHeaderSize (ncgPlatform config) + spillSlotSize * slot

-- | Get the registers that are being used by this instruction.
-- regUsage doesn't need to do any trickery for jumps and such.
-- Just state precisely the regs read and written by that insn.
-- The consequences of control flow transfers, as far as register
-- allocation goes, are taken care of by the register allocator.
--
-- RegUsage = RU [<read regs>] [<write regs>]

instance Outputable RegUsage where
    ppr (RU reads writes) = text "RegUsage(reads:" <+> ppr reads <> comma <+> text "writes:" <+> ppr writes <> char ')'

regUsageOfInstr :: Platform -> Instr -> RegUsage
regUsageOfInstr platform instr = case instr of
  ANN _ i                  -> regUsageOfInstr platform i
  COMMENT{}                -> usage ([], [])
  MULTILINE_COMMENT{}      -> usage ([], [])
  PUSH_STACK_FRAME         -> usage ([], [])
  POP_STACK_FRAME          -> usage ([], [])
  DELTA{}                  -> usage ([], [])

  -- 1. Arithmetic Instructions ------------------------------------------------
  ADD dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  -- CMN l r                  -> usage (regOp l ++ regOp r, [])
  -- CMP l r                  -> usage (regOp l ++ regOp r, [])
  MUL dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  NEG dst src              -> usage (regOp src, regOp dst)
  SMULH dst src1 src2      -> usage (regOp src1 ++ regOp src2, regOp dst)
  SMULL dst src1 src2      -> usage (regOp src1 ++ regOp src2, regOp dst)
  DIV dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  REM dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  SUB dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  DIVU dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)

  -- 2. Bit Manipulation Instructions ------------------------------------------
  SBFM dst src _ _         -> usage (regOp src, regOp dst)
  UBFM dst src _ _         -> usage (regOp src, regOp dst)
  UBFX dst src _ _         -> usage (regOp src, regOp dst)
  -- 3. Logical and Move Instructions ------------------------------------------
  AND dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  OR dst src1 src2         -> usage (regOp src1 ++ regOp src2, regOp dst)
  ASR dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  BIC dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  BICS dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)
  XOR dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  LSL dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  LSR dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  MOV dst src              -> usage (regOp src, regOp dst)
  MOVK dst src             -> usage (regOp src, regOp dst)
  -- ORI's third operand is always an immediate
  ORI dst src1 _           -> usage (regOp src1, regOp dst)
  XORI dst src1 _          -> usage (regOp src1, regOp dst)
  ROR dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  TST src1 src2            -> usage (regOp src1 ++ regOp src2, [])
  -- 4. Branch Instructions ----------------------------------------------------
  J t                      -> usage (regTarget t, [])
  B t                      -> usage (regTarget t, [])
  BCOND _ l r t            -> usage (regTarget t ++ regOp l ++ regOp r, [])
  BL t ps _rs              -> usage (regTarget t ++ ps, callerSavedRegisters)

  -- 5. Atomic Instructions ----------------------------------------------------
  -- 6. Conditional Instructions -----------------------------------------------
  CSET dst l r _           -> usage (regOp l ++ regOp r, regOp dst)
  CBZ src _                -> usage (regOp src, [])
  CBNZ src _               -> usage (regOp src, [])
  -- 7. Load and Store Instructions --------------------------------------------
  STR _ src dst            -> usage (regOp src ++ regOp dst, [])
  -- STLR _ src dst      L     -> usage (regOp src ++ regOp dst, [])
  LDR _ dst src            -> usage (regOp src, regOp dst)
  -- LDAR _ dst src           -> usage (regOp src, regOp dst)
  -- TODO is this right? see STR, which I'm only partial about being right?
  -- STP _ src1 src2 dst      -> usage (regOp src1 ++ regOp src2 ++ regOp dst, [])
  -- LDP _ dst1 dst2 src      -> usage (regOp src, regOp dst1 ++ regOp dst2)

  -- 8. Synchronization Instructions -------------------------------------------
  DMBSY _ _                  -> usage ([], [])

  -- 9. Floating Point Instructions --------------------------------------------
  FCVT dst src             -> usage (regOp src, regOp dst)
  SCVTF dst src            -> usage (regOp src, regOp dst)
  FCVTZS dst src           -> usage (regOp src, regOp dst)
  FABS dst src             -> usage (regOp src, regOp dst)

  _ -> panic $ "regUsageOfInstr: " ++ instrCon instr

  where
        -- filtering the usage is necessary, otherwise the register
        -- allocator will try to allocate pre-defined fixed stg
        -- registers as well, as they show up.
        usage (src, dst) = RU (filter (interesting platform) src)
                              (filter (interesting platform) dst)

        regAddr :: AddrMode -> [Reg]
        regAddr (AddrRegReg r1 r2) = [r1, r2]
        regAddr (AddrRegImm r1 _)  = [r1]
        regAddr (AddrReg r1)       = [r1]
        regOp :: Operand -> [Reg]
        regOp (OpReg _ r1) = [r1]
        regOp (OpRegExt _ r1 _ _) = [r1]
        regOp (OpRegShift _ r1 _ _) = [r1]
        regOp (OpAddr a) = regAddr a
        regOp (OpImm _) = []
        regOp (OpImmShift _ _ _) = []
        regTarget :: Target -> [Reg]
        regTarget (TBlock _) = []
        regTarget (TLabel _) = []
        regTarget (TReg r1)  = [r1]

        -- Is this register interesting for the register allocator?
        interesting :: Platform -> Reg -> Bool
        interesting _        (RegVirtual _)                 = True
        interesting _        (RegReal (RealRegSingle (-1))) = False
        interesting platform (RegReal (RealRegSingle i))    = freeReg platform i

-- Save caller save registers
-- This is x0-x18
--
-- For SIMD/FP Registers:
-- Registers v8-v15 must be preserved by a callee across subroutine calls;
-- the remaining registers (v0-v7, v16-v31) do not need to be preserved (or
-- should be preserved by the caller). Additionally, only the bottom 64 bits
-- of each value stored in v8-v15 need to be preserved [7]; it is the
-- responsibility of the caller to preserve larger values.
--
-- .---------------------------------------------------------------------------------------------------------------------------------------------------------------.
-- |  0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 |
-- | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 | 41 | 42 | 42 | 44 | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 |
-- |== General Purpose registers ==================================================================================================================================|
-- | ZR | RA | SP | GP | TP | <- tmp r. -> | FP | <- | <---- argument passing -------------> | -- callee saved ------------------------------> | <--- tmp regs --> |
-- | -- | -- | -- | -- | -- | <- free r. > | -- | BR | <---- free registers ---------------> | SP | HP | R1 | R2 | R3 | R4 | R5 | R6 | R7 | SL | <-- free regs --> |
-- |== SIMD/FP Registers ==========================================================================================================================================|
-- | <--- temporary registers -----------> | <------ | <---- argument passing -------------> | -- callee saved ------------------------------> | <--- tmp regs --> |
-- | <---------- free registers ---------> | F1 | F2 | <---- free registers ---------------> | F3 | F4 | F5 | F6 | D1 | D2 | D3 | D4 | D5 | D6 | -- | -- | -- | -- |
-- '---------------------------------------------------------------------------------------------------------------------------------------------------------------'
-- ZR: Zero, RA: Return Address, SP: Stack Pointer, GP: Global Pointer, TP: Thread Pointer, FP: Frame Pointer
-- BR: Base, SL: SpLim
callerSavedRegisters :: [Reg]
callerSavedRegisters
    = map regSingle [5..7]
    ++ map regSingle [10..17]
    ++ map regSingle [28..31]
    ++ map regSingle [32..39]
    ++ map regSingle [42..49]

-- | Apply a given mapping to all the register references in this
-- instruction.
patchRegsOfInstr :: Instr -> (Reg -> Reg) -> Instr
patchRegsOfInstr instr env = case instr of
    -- 0. Meta Instructions
    ANN d i             -> ANN d (patchRegsOfInstr i env)
    COMMENT{}           -> instr
    MULTILINE_COMMENT{} -> instr
    PUSH_STACK_FRAME    -> instr
    POP_STACK_FRAME     -> instr
    DELTA{}             -> instr
    -- 1. Arithmetic Instructions ----------------------------------------------
    ADD o1 o2 o3   -> ADD (patchOp o1) (patchOp o2) (patchOp o3)
    -- CMN o1 o2      -> CMN (patchOp o1) (patchOp o2)
    -- CMP o1 o2      -> CMP (patchOp o1) (patchOp o2)
    MUL o1 o2 o3   -> MUL (patchOp o1) (patchOp o2) (patchOp o3)
    NEG o1 o2      -> NEG (patchOp o1) (patchOp o2)
    SMULH o1 o2 o3 -> SMULH (patchOp o1) (patchOp o2)  (patchOp o3)
    SMULL o1 o2 o3 -> SMULL (patchOp o1) (patchOp o2)  (patchOp o3)
    DIV o1 o2 o3   -> DIV (patchOp o1) (patchOp o2) (patchOp o3)
    REM o1 o2 o3   -> REM (patchOp o1) (patchOp o2) (patchOp o3)
    SUB o1 o2 o3   -> SUB  (patchOp o1) (patchOp o2) (patchOp o3)
    DIVU o1 o2 o3  -> DIVU (patchOp o1) (patchOp o2) (patchOp o3)

    -- 2. Bit Manipulation Instructions ----------------------------------------
    SBFM o1 o2 o3 o4 -> SBFM (patchOp o1) (patchOp o2) (patchOp o3) (patchOp o4)
    UBFM o1 o2 o3 o4 -> UBFM (patchOp o1) (patchOp o2) (patchOp o3) (patchOp o4)
    UBFX o1 o2 o3 o4 -> UBFX (patchOp o1) (patchOp o2) (patchOp o3) (patchOp o4)

    -- 3. Logical and Move Instructions ----------------------------------------
    AND o1 o2 o3   -> AND  (patchOp o1) (patchOp o2) (patchOp o3)
    OR o1 o2 o3    -> OR   (patchOp o1) (patchOp o2) (patchOp o3)
    -- ANDS o1 o2 o3  -> ANDS (patchOp o1) (patchOp o2) (patchOp o3)
    ASR o1 o2 o3   -> ASR  (patchOp o1) (patchOp o2) (patchOp o3)
    BIC o1 o2 o3   -> BIC  (patchOp o1) (patchOp o2) (patchOp o3)
    BICS o1 o2 o3  -> BICS (patchOp o1) (patchOp o2) (patchOp o3)
    XOR o1 o2 o3   -> XOR  (patchOp o1) (patchOp o2) (patchOp o3)
    LSL o1 o2 o3   -> LSL  (patchOp o1) (patchOp o2) (patchOp o3)
    LSR o1 o2 o3   -> LSR  (patchOp o1) (patchOp o2) (patchOp o3)
    MOV o1 o2      -> MOV  (patchOp o1) (patchOp o2)
    MOVK o1 o2     -> MOVK (patchOp o1) (patchOp o2)
    -- o3 cannot be a register for ORI (always an immediate)
    ORI o1 o2 o3   -> ORI  (patchOp o1) (patchOp o2) (patchOp o3)
    XORI o1 o2 o3  -> XORI  (patchOp o1) (patchOp o2) (patchOp o3)
    ROR o1 o2 o3   -> ROR  (patchOp o1) (patchOp o2) (patchOp o3)
    TST o1 o2      -> TST  (patchOp o1) (patchOp o2)

    -- 4. Branch Instructions --------------------------------------------------
    J t            -> J (patchTarget t)
    B t            -> B (patchTarget t)
    BL t rs ts     -> BL (patchTarget t) rs ts
    BCOND c o1 o2 t -> BCOND c (patchOp o1) (patchOp o2) (patchTarget t)

    -- 5. Atomic Instructions --------------------------------------------------
    -- 6. Conditional Instructions ---------------------------------------------
    CSET o l r c   -> CSET (patchOp o) (patchOp l) (patchOp r) c
    CBZ o l        -> CBZ (patchOp o) l
    CBNZ o l       -> CBNZ (patchOp o) l
    -- 7. Load and Store Instructions ------------------------------------------
    STR f o1 o2    -> STR f (patchOp o1) (patchOp o2)
    -- STLR f o1 o2   -> STLR f (patchOp o1) (patchOp o2)
    LDR f o1 o2    -> LDR f (patchOp o1) (patchOp o2)
    -- LDAR f o1 o2   -> LDAR f (patchOp o1) (patchOp o2)
    -- STP f o1 o2 o3 -> STP f (patchOp o1) (patchOp o2) (patchOp o3)
    -- LDP f o1 o2 o3 -> LDP f (patchOp o1) (patchOp o2) (patchOp o3)

    -- 8. Synchronization Instructions -----------------------------------------
    DMBSY o1 o2    -> DMBSY o1 o2

    -- 9. Floating Point Instructions ------------------------------------------
    FCVT o1 o2     -> FCVT (patchOp o1) (patchOp o2)
    SCVTF o1 o2    -> SCVTF (patchOp o1) (patchOp o2)
    FCVTZS o1 o2   -> FCVTZS (patchOp o1) (patchOp o2)
    FABS o1 o2     -> FABS (patchOp o1) (patchOp o2)
    _              -> panic $ "patchRegsOfInstr: " ++ instrCon instr
    where
        patchOp :: Operand -> Operand
        patchOp (OpReg w r) = OpReg w (env r)
        patchOp (OpRegExt w r x s) = OpRegExt w (env r) x s
        patchOp (OpRegShift w r m s) = OpRegShift w (env r) m s
        patchOp (OpAddr a) = OpAddr (patchAddr a)
        patchOp op = op
        patchTarget :: Target -> Target
        patchTarget (TReg r) = TReg (env r)
        patchTarget t = t
        patchAddr :: AddrMode -> AddrMode
        patchAddr (AddrRegReg r1 r2) = AddrRegReg (env r1) (env r2)
        patchAddr (AddrRegImm r1 i)  = AddrRegImm (env r1) i
        patchAddr (AddrReg r) = AddrReg (env r)
--------------------------------------------------------------------------------
-- | Checks whether this instruction is a jump/branch instruction.
-- One that can change the flow of control in a way that the
-- register allocator needs to worry about.
isJumpishInstr :: Instr -> Bool
isJumpishInstr instr = case instr of
    ANN _ i -> isJumpishInstr i
    CBZ{} -> True
    CBNZ{} -> True
    J{} -> True
    B{} -> True
    BL{} -> True
    BCOND{} -> True
    _ -> False

-- | Checks whether this instruction is a jump/branch instruction.
-- One that can change the flow of control in a way that the
-- register allocator needs to worry about.
jumpDestsOfInstr :: Instr -> [BlockId]
jumpDestsOfInstr (ANN _ i) = jumpDestsOfInstr i
jumpDestsOfInstr (CBZ _ t) = [ id | TBlock id <- [t]]
jumpDestsOfInstr (CBNZ _ t) = [ id | TBlock id <- [t]]
jumpDestsOfInstr (J t) = [id | TBlock id <- [t]]
jumpDestsOfInstr (B t) = [id | TBlock id <- [t]]
jumpDestsOfInstr (BL t _ _) = [ id | TBlock id <- [t]]
jumpDestsOfInstr (BCOND _ _ _ t) = [ id | TBlock id <- [t]]
jumpDestsOfInstr _ = []

-- | Change the destination of this jump instruction.
-- Used in the linear allocator when adding fixup blocks for join
-- points.
patchJumpInstr :: Instr -> (BlockId -> BlockId) -> Instr
patchJumpInstr instr patchF
    = case instr of
        ANN d i -> ANN d (patchJumpInstr i patchF)
        CBZ r (TBlock bid) -> CBZ r (TBlock (patchF bid))
        CBNZ r (TBlock bid) -> CBNZ r (TBlock (patchF bid))
        J (TBlock bid) -> J (TBlock (patchF bid))
        B (TBlock bid) -> B (TBlock (patchF bid))
        BL (TBlock bid) ps rs -> BL (TBlock (patchF bid)) ps rs
        BCOND c o1 o2 (TBlock bid) -> BCOND c o1 o2 (TBlock (patchF bid))
        _ -> panic $ "patchJumpInstr: " ++ instrCon instr

-- -----------------------------------------------------------------------------
-- Note [Spills and Reloads]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- We reserve @RESERVED_C_STACK_BYTES@ on the C stack for spilling and reloading
-- registers.  AArch64s maximum displacement for SP relative spills and reloads
-- is essentially [-256,255], or [0, 0xFFF]*8 = [0, 32760] for 64bits.
--
-- The @RESERVED_C_STACK_BYTES@ is 16k, so we can't address any location in a
-- single instruction.  The idea is to use the Inter Procedure 0 (ip) register
-- to perform the computations for larger offsets.
--
-- Using sp to compute the offset will violate assumptions about the stack pointer
-- pointing to the top of the stack during signal handling.  As we can't force
-- every signal to use its own stack, we have to ensure that the stack pointer
-- always points to the top of the stack, and we can't use it for computation.
--
-- | An instruction to spill a register into a spill slot.
mkSpillInstr ::
  HasCallStack =>
  NCGConfig ->
  Reg -> -- register to spill
  Int -> -- current stack delta
  Int -> -- spill slot to use
  [Instr]
mkSpillInstr config reg delta slot =
  case off - delta of
    imm | fitsIn12bitImm imm -> [mkStrSpImm imm]
    imm ->
      [ movImmToIp imm,
        addSpToIp,
        mkStrIp
      ]
  where
    fmt = case reg of
      RegReal (RealRegSingle n) | n < 32 -> II64
      _ -> FF64
    mkStrSpImm imm = ANN (text "Spill@" <> int (off - delta)) $ STR fmt (OpReg W64 reg) (OpAddr (AddrRegImm sp_reg (ImmInt imm)))
    movImmToIp imm = ANN (text "Spill: IP <- " <> int imm) $ MOV ip (OpImm (ImmInt imm))
    addSpToIp = ANN (text "Spill: IP <- SP + IP ") $ ADD ip ip sp
    mkStrIp = ANN (text "Spill@" <> int (off - delta)) $ STR fmt (OpReg W64 reg) (OpAddr (AddrReg ip_reg))

    off = spillSlotToOffset config slot

mkLoadInstr
   :: NCGConfig
   -> Reg       -- register to load
   -> Int       -- current stack delta
   -> Int       -- spill slot to use
   -> [Instr]

mkLoadInstr config reg delta slot =
  case off - delta of
    imm | fitsIn12bitImm imm -> [mkLdrSpImm imm]
    imm ->
      [ movImmToIp imm,
        addSpToIp,
        mkLdrIp
      ]
  where
    fmt = case reg of
      RegReal (RealRegSingle n) | n < 32 -> II64
      _ -> FF64
    mkLdrSpImm imm = ANN (text "Reload@" <> int (off - delta)) $ LDR fmt (OpReg W64 reg) (OpAddr (AddrRegImm sp_reg (ImmInt imm)))
    movImmToIp imm = ANN (text "Reload: IP <- " <> int imm) $ MOV ip (OpImm (ImmInt imm))
    addSpToIp = ANN (text "Reload: IP <- SP + IP ") $ ADD ip ip sp
    mkLdrIp = ANN (text "Reload@" <> int (off - delta)) $ LDR fmt (OpReg W64 reg) (OpAddr (AddrReg ip_reg))

    off = spillSlotToOffset config slot

  --------------------------------------------------------------------------------
-- | See if this instruction is telling us the current C stack delta
takeDeltaInstr :: Instr -> Maybe Int
takeDeltaInstr (ANN _ i) = takeDeltaInstr i
takeDeltaInstr (DELTA i) = Just i
takeDeltaInstr _         = Nothing

-- Not real instructions.  Just meta data
isMetaInstr :: Instr -> Bool
isMetaInstr instr
 = case instr of
    ANN _ i     -> isMetaInstr i
    COMMENT{}   -> True
    MULTILINE_COMMENT{} -> True
    LOCATION{}  -> True
    LDATA{}     -> True
    NEWBLOCK{}  -> True
    DELTA{}     -> True
    PUSH_STACK_FRAME -> True
    POP_STACK_FRAME -> True
    _           -> False

-- | Copy the value in a register to another one.
-- Must work for all register classes.
mkRegRegMoveInstr :: Reg -> Reg -> Instr
mkRegRegMoveInstr src dst = ANN (text "Reg->Reg Move: " <> ppr src <> text " -> " <> ppr dst) $ MOV (OpReg W64 dst) (OpReg W64 src)

-- | Take the source and destination from this reg -> reg move instruction
-- or Nothing if it's not one
takeRegRegMoveInstr :: Instr -> Maybe (Reg,Reg)
--takeRegRegMoveInstr (MOV (OpReg fmt dst) (OpReg fmt' src)) | fmt == fmt' = Just (src, dst)
takeRegRegMoveInstr _ = Nothing

-- | Make an unconditional jump instruction.
mkJumpInstr :: BlockId -> [Instr]
mkJumpInstr id = [B (TBlock id)]

mkStackAllocInstr :: Platform -> Int -> [Instr]
mkStackAllocInstr platform n
    | n == 0 = []
    | n > 0 && fitsIn12bitImm n = [ ANN (text "Alloc More Stack") $ SUB sp sp (OpImm (ImmInt n)) ]
    -- TODO: This case may be optimized with the IP register for large n-s
    | n > 0 =  ANN (text "Alloc More Stack") (SUB sp sp (OpImm (ImmInt intMax12bit))) : mkStackAllocInstr platform (n - intMax12bit)
mkStackAllocInstr _platform n = pprPanic "mkStackAllocInstr" (int n)

mkStackDeallocInstr :: Platform -> Int -> [Instr]
mkStackDeallocInstr platform n
    | n == 0 = []
    | n > 0 && fitsIn12bitImm n = [ ANN (text "Dealloc More Stack") $ ADD sp sp (OpImm (ImmInt n)) ]
    -- TODO: This case may be optimized with the IP register for large n-s
    | n > 0 =  ANN (text "Dealloc More Stack") (ADD sp sp (OpImm (ImmInt intMax12bit))) : mkStackDeallocInstr platform (n - intMax12bit)
mkStackDeallocInstr _platform n = pprPanic "mkStackDeallocInstr" (int n)

--
-- See Note [extra spill slots] in X86/Instr.hs
--
allocMoreStack
  :: Platform
  -> Int
  -> NatCmmDecl statics GHC.CmmToAsm.RV64.Instr.Instr
  -> UniqSM (NatCmmDecl statics GHC.CmmToAsm.RV64.Instr.Instr, [(BlockId,BlockId)])

allocMoreStack _ _ top@(CmmData _ _) = return (top,[])
allocMoreStack platform slots proc@(CmmProc info lbl live (ListGraph code)) = do
    let entries = entryBlocks proc

    uniqs <- getUniquesM

    let
      delta = ((x + stackAlign - 1) `quot` stackAlign) * stackAlign -- round up
        where x = slots * spillSlotSize -- sp delta

      alloc   = mkStackAllocInstr   platform delta
      dealloc = mkStackDeallocInstr platform delta

      retargetList = (zip entries (map mkBlockId uniqs))

      new_blockmap :: LabelMap BlockId
      new_blockmap = mapFromList retargetList

      insert_stack_insn (BasicBlock id insns)
        | Just new_blockid <- mapLookup id new_blockmap
        = [ BasicBlock id $ alloc ++ [ B (TBlock new_blockid) ]
          , BasicBlock new_blockid block' ]
        | otherwise
        = [ BasicBlock id block' ]
        where
          block' = foldr insert_dealloc [] insns

      insert_dealloc insn r = case insn of
        J _ -> dealloc ++ (insn : r)
        ANN _ (J _) -> dealloc ++ (insn : r)
        _other | jumpDestsOfInstr insn /= []
            -> patchJumpInstr insn retarget : r
        _other -> insn : r

        where retarget b = fromMaybe b (mapLookup b new_blockmap)

      new_code = concatMap insert_stack_insn code
    -- in
    return (CmmProc info lbl live (ListGraph new_code), retargetList)
-- -----------------------------------------------------------------------------
-- Machine's assembly language

-- We have a few common "instructions" (nearly all the pseudo-ops) but
-- mostly all of 'Instr' is machine-specific.

-- Some additional (potential future) instructions are commented out. They are
-- not needed yet for the backend but could be used in the future.

-- RV64 reference card: https://cs61c.org/sp23/pdfs/resources/reference-card.pdf
-- RV64 pseudo instructions: https://github.com/riscv-non-isa/riscv-asm-manual/blob/master/riscv-asm.md#-a-listing-of-standard-risc-v-pseudoinstructions
-- We will target: RV64G(C). That is G = I+A+F+S+D
-- I: Integer Multiplication and Division
-- A: Atomic Instructions
-- F: Single Precision
-- D: Double Precision
-- C: Compressed (though we won't use that).

-- This most notably leaves out B. (Bit Manipulation) instructions.

data Instr
    -- comment pseudo-op
    = COMMENT SDoc
    | MULTILINE_COMMENT SDoc

    -- Annotated instruction. Should print <instr> # <doc>
    | ANN SDoc Instr

    -- location pseudo-op (file, line, col, name)
    | LOCATION Int Int Int String

    -- some static data spat out during code
    -- generation.  Will be extracted before
    -- pretty-printing.
    | LDATA   Section RawCmmStatics

    -- start a new basic block.  Useful during
    -- codegen, removed later.  Preceding
    -- instruction should be a jump, as per the
    -- invariants for a BasicBlock (see Cmm).
    | NEWBLOCK BlockId

    -- specify current stack offset for
    -- benefit of subsequent passes
    | DELTA   Int

    -- 0. Pseudo Instructions --------------------------------------------------
    -- | SXTW Operand Operand
    -- | SXTX Operand Operand
    | PUSH_STACK_FRAME
    | POP_STACK_FRAME

    -- == Base Instructions (I) ================================================
    -- 1. Arithmetic Instructions ----------------------------------------------
    -- all of these instructions can also take an immediate, in which case they
    -- hafe a suffix I (except for U suffix, where it's IU then. E.g. SLTIU).
    | ADD Operand Operand Operand -- rd = rs1 + rs2
    | SUB Operand Operand Operand -- rd = rs1 - rs2

    | AND Operand Operand Operand -- rd = rs1 & rs2
    | OR  Operand Operand Operand -- rd = rs1 | rs2
    -- | XOR Operand Operand Operand -- rd = rs1 ^ rs2
    | LSL {- SLL -} Operand Operand Operand -- rd = rs1 << rs2 (zero ext)
    | LSR {- SRL -} Operand Operand Operand -- rd = rs1 >> rs2 (zero ext)
    -- | ASL {- SLA -} Operand Operand Operand -- rd = rs1 << rs2 (sign ext)
    | ASR {- SRA -} Operand Operand Operand -- rd = rs1 >> rs2 (sign ext)
    -- | SLT Operand Operand Operand -- rd = rs1 < rs2 ? 1 : 0 (signed)
    -- | SLTU Operand Operand Operand -- rd = rs1 < rs2 ? 1 : 0 (unsigned)

    -- 2. Memory Load/Store Instructions ---------------------------------------
    -- Unlike arm, we don't have register shorthands for size.
    -- We do however have {L,S}{B,H,W,D}[U] instructions for Load/Store, Byte, Half, Word, Double, (Unsigned).
    -- Reusing the arm logic with the _format_ specifier will hopefully work.
    | STR Format Operand Operand -- str Xn, address-mode // Xn -> *addr
    | LDR Format Operand Operand -- ldr Xn, address-mode // Xn <- *addr

    -- 3. Control Flow ---------------------------------------------------------
    -- B{EQ,GE,GEU,LT,LTU}, these are effectively BCOND from AArch64;
    -- however, AArch64 desugars them into CMP + BCOND. So these are a bit more
    -- powerful.
    -- JAL / JARL are effectively the BL instruction from AArch64.


    -- | CMN Operand Operand -- rd + op2
    -- | CMP Operand Operand -- rd - op2

    | MUL Operand Operand Operand -- rd = rn × rm


    -- Pseudo/synthesized:
    -- NEG = SUB x, 0, y
    -- NOT = XOR -1, x
    | NEG Operand Operand -- rd = -op2

    | DIV Operand Operand Operand -- rd = rn ÷ rm
    | REM Operand Operand Operand -- rd = rn % rm

    -- TODO: Rename: MULH
    | SMULH Operand Operand Operand
    | SMULL Operand Operand Operand

    | DIVU Operand Operand Operand -- rd = rn ÷ rm

    -- 2. Bit Manipulation Instructions ----------------------------------------
    | SBFM Operand Operand Operand Operand -- rd = rn[i,j]
    | UBFM Operand Operand Operand Operand -- rd = rn[i,j]
    -- Signed/Unsigned bitfield extract
    | UBFX Operand Operand Operand Operand -- rd = rn[i,j]

    -- 3. Logical and Move Instructions ----------------------------------------
    -- | AND Operand Operand Operand -- rd = rn & op2
    -- | ANDS Operand Operand Operand -- rd = rn & op2
    -- | ASR Operand Operand Operand -- rd = rn ≫ rm  or  rd = rn ≫ #i, i is 6 bits
    | BIC Operand Operand Operand -- rd = rn & ~op2
    | BICS Operand Operand Operand -- rd = rn & ~op2
    | XOR Operand Operand Operand -- rd = rn ⊕ op2
    -- | LSL Operand Operand Operand -- rd = rn ≪ rm  or rd = rn ≪ #i, i is 6 bits
    -- | LSR Operand Operand Operand -- rd = rn ≫ rm  or rd = rn ≫ #i, i is 6 bits
    | MOV Operand Operand -- rd = rn  or  rd = #i
    | MOVK Operand Operand
    -- | MOVN Operand Operand
    -- | MOVZ Operand Operand
    | ORN Operand Operand Operand -- rd = rn | ~op2
    | ORI Operand Operand Operand -- rd = rn | op2
    | XORI Operand Operand Operand -- rd = rn `xor` imm
    | ROR Operand Operand Operand -- rd = rn ≫ rm  or  rd = rn ≫ #i, i is 6 bits
    | TST Operand Operand -- rn & op2
    -- Load and stores.
    -- TODO STR/LDR might want to change to STP/LDP with XZR for the second register.
    -- | STR Format Operand Operand -- str Xn, address-mode // Xn -> *addr
    -- | STLR Format Operand Operand -- stlr Xn, address-mode // Xn -> *addr
    -- | LDR Format Operand Operand -- ldr Xn, address-mode // Xn <- *addr
    -- | LDAR Format Operand Operand -- ldar Xn, address-mode // Xn <- *addr
    -- | STP Format Operand Operand Operand -- stp Xn, Xm, address-mode // Xn -> *addr, Xm -> *(addr + 8)
    -- | LDP Format Operand Operand Operand -- stp Xn, Xm, address-mode // Xn <- *addr, Xm <- *(addr + 8)

    -- Conditional instructions
    -- This is a synthetic operation.
    | CSET Operand Operand Operand Cond   -- if(o2 cond o3) op <- 1 else op <- 0

    | CBZ Operand Target  -- if op == 0, then branch.
    | CBNZ Operand Target -- if op /= 0, then branch.
    -- Branching.
    | J Target            -- like B, but only generated from genJump. Used to distinguish genJumps from others.
    | B Target            -- unconditional branching b/br. (To a blockid, label or register)
    | BL Target [Reg] [Reg] -- branch and link (e.g. set x30 to next pc, and branch)
    | BCOND Cond Operand Operand Target   -- branch with condition. b.<cond>

    -- 8. Synchronization Instructions -----------------------------------------
    | DMBSY DmbType DmbType
    -- 9. Floating Point Instructions
    -- Float ConVerT
    | FCVT Operand Operand
    -- Signed ConVerT Float
    | SCVTF Operand Operand
    -- Float ConVerT to Zero Signed
    | FCVTZS Operand Operand
    -- Float ABSolute value
    | FABS Operand Operand

data DmbType = DmbRead | DmbWrite | DmbReadWrite

instrCon :: Instr -> String
instrCon i =
    case i of
      COMMENT{} -> "COMMENT"
      MULTILINE_COMMENT{} -> "COMMENT"
      ANN{} -> "ANN"
      LOCATION{} -> "LOCATION"
      LDATA{} -> "LDATA"
      NEWBLOCK{} -> "NEWBLOCK"
      DELTA{} -> "DELTA"
      PUSH_STACK_FRAME{} -> "PUSH_STACK_FRAME"
      POP_STACK_FRAME{} -> "POP_STACK_FRAME"
      ADD{} -> "ADD"
      OR{} -> "OR"
      -- CMN{} -> "CMN"
      -- CMP{} -> "CMP"
      MUL{} -> "MUL"
      NEG{} -> "NEG"
      DIV{} -> "DIV"
      REM{} -> "REM"
      SMULH{} -> "SMULH"
      SMULL{} -> "SMULL"
      SUB{} -> "SUB"
      DIVU{} -> "DIVU"
      SBFM{} -> "SBFM"
      UBFM{} -> "UBFM"
      UBFX{} -> "UBFX"
      AND{} -> "AND"
      -- ANDS{} -> "ANDS"
      ASR{} -> "ASR"
      BIC{} -> "BIC"
      BICS{} -> "BICS"
      XOR{} -> "XOR"
      LSL{} -> "LSL"
      LSR{} -> "LSR"
      MOV{} -> "MOV"
      MOVK{} -> "MOVK"
      ORN{} -> "ORN"
      ORI{} -> "ORI"
      XORI{} -> "ORI"
      ROR{} -> "ROR"
      TST{} -> "TST"
      STR{} -> "STR"
      -- STLR{} -> "STLR"
      LDR{} -> "LDR"
      -- LDAR{} -> "LDAR"
      -- STP{} -> "STP"
      -- LDP{} -> "LDP"
      CSET{} -> "CSET"
      CBZ{} -> "CBZ"
      CBNZ{} -> "CBNZ"
      J{} -> "J"
      B{} -> "B"
      BL{} -> "BL"
      BCOND{} -> "BCOND"
      DMBSY{} -> "DMBSY"
      FCVT{} -> "FCVT"
      SCVTF{} -> "SCVTF"
      FCVTZS{} -> "FCVTZS"
      FABS{} -> "FABS"

data Target
    = TBlock BlockId
    | TLabel CLabel
    | TReg   Reg


-- Extension
-- {Unsigned|Signed}XT{Byte|Half|Word|Doube}
data ExtMode
    = EUXTB | EUXTH | EUXTW | EUXTX
    | ESXTB | ESXTH | ESXTW | ESXTX
    deriving (Eq, Show)

data ShiftMode
    = SLSL | SLSR | SASR | SROR
    deriving (Eq, Show)


-- We can also add ExtShift to Extension.
-- However at most 3bits.
type ExtShift = Int
-- at most 6bits
type RegShift = Int

data Operand
        = OpReg Width Reg            -- register
        | OpRegExt Width Reg ExtMode ExtShift -- rm, <ext>[, <shift left>]
        | OpRegShift Width Reg ShiftMode RegShift     -- rm, <shift>, <0-64>
        | OpImm Imm            -- immediate value
        -- TODO: Does OpImmShift exist in RV64?
        | OpImmShift Imm ShiftMode RegShift
        | OpAddr AddrMode       -- memory reference
        deriving (Eq, Show)

-- Smart constructors
opReg :: Width -> Reg -> Operand
opReg = OpReg

-- Note [The made-up RISCV64 IP register]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- RISCV64 has no inter-procedural register in its ABI. However, we need one to
-- make register spills/loads to/from high number slots. I.e. slot numbers that
-- do not fit in a 12bit integer which is used as immediate in the arithmetic
-- operations. Thus, we're marking one additional register (x31) as permanently
-- non-free and call it IP.
--
-- IP can be used as temporary register in all operations. Just be aware that it
-- may be clobbered as soon as you loose direct control over it (i.e. using IP
-- by-passes the register allocation/spilling mechanisms.) It should be fine to
-- use it as temporary register in a MachOp translation as long as you don't
-- rely on its value beyond this limited scope.
--
-- X31 is a caller-saved register. I.e. there are no guarantees about what the
-- callee does with it. That's exactly what we want here.

zero_reg, ra_reg, sp_reg, ip_reg :: Reg
zero_reg = RegReal (RealRegSingle 0)
ra_reg = RegReal (RealRegSingle 1)
sp_reg = RegReal (RealRegSingle 2)
ip_reg = RegReal (RealRegSingle 31)

zero, ra, sp, gp, tp, fp, ip :: Operand
zero = OpReg W64 zero_reg
ra  = OpReg W64 ra_reg
sp  = OpReg W64 sp_reg
gp  = OpReg W64 (RegReal (RealRegSingle 3))
tp  = OpReg W64 (RegReal (RealRegSingle 4))
fp  = OpReg W64 (RegReal (RealRegSingle 8))
ip = OpReg W64 ip_reg

_x :: Int -> Operand
_x i = OpReg W64 (RegReal (RealRegSingle i))
x0,  x1,  x2,  x3,  x4,  x5,  x6,  x7  :: Operand
x8,  x9,  x10, x11, x12, x13, x14, x15 :: Operand
x16, x17, x18, x19, x20, x21, x22, x23 :: Operand
x24, x25, x26, x27, x28, x29, x30, x31 :: Operand
x0  = OpReg W64 (RegReal (RealRegSingle  0))
x1  = OpReg W64 (RegReal (RealRegSingle  1))
x2  = OpReg W64 (RegReal (RealRegSingle  2))
x3  = OpReg W64 (RegReal (RealRegSingle  3))
x4  = OpReg W64 (RegReal (RealRegSingle  4))
x5  = OpReg W64 (RegReal (RealRegSingle  5))
x6  = OpReg W64 (RegReal (RealRegSingle  6))
x7  = OpReg W64 (RegReal (RealRegSingle  7))
x8  = OpReg W64 (RegReal (RealRegSingle  8))
x9  = OpReg W64 (RegReal (RealRegSingle  9))
x10 = OpReg W64 (RegReal (RealRegSingle 10))
x11 = OpReg W64 (RegReal (RealRegSingle 11))
x12 = OpReg W64 (RegReal (RealRegSingle 12))
x13 = OpReg W64 (RegReal (RealRegSingle 13))
x14 = OpReg W64 (RegReal (RealRegSingle 14))
x15 = OpReg W64 (RegReal (RealRegSingle 15))
x16 = OpReg W64 (RegReal (RealRegSingle 16))
x17 = OpReg W64 (RegReal (RealRegSingle 17))
x18 = OpReg W64 (RegReal (RealRegSingle 18))
x19 = OpReg W64 (RegReal (RealRegSingle 19))
x20 = OpReg W64 (RegReal (RealRegSingle 20))
x21 = OpReg W64 (RegReal (RealRegSingle 21))
x22 = OpReg W64 (RegReal (RealRegSingle 22))
x23 = OpReg W64 (RegReal (RealRegSingle 23))
x24 = OpReg W64 (RegReal (RealRegSingle 24))
x25 = OpReg W64 (RegReal (RealRegSingle 25))
x26 = OpReg W64 (RegReal (RealRegSingle 26))
x27 = OpReg W64 (RegReal (RealRegSingle 27))
x28 = OpReg W64 (RegReal (RealRegSingle 28))
x29 = OpReg W64 (RegReal (RealRegSingle 29))
x30 = OpReg W64 (RegReal (RealRegSingle 30))
x31 = OpReg W64 (RegReal (RealRegSingle 31))

_d :: Int -> Operand
_d = OpReg W64 . RegReal . RealRegSingle
d0,  d1,  d2,  d3,  d4,  d5,  d6,  d7  :: Operand
d8,  d9,  d10, d11, d12, d13, d14, d15 :: Operand
d16, d17, d18, d19, d20, d21, d22, d23 :: Operand
d24, d25, d26, d27, d28, d29, d30, d31 :: Operand
d0  = OpReg W64 (RegReal (RealRegSingle 32))
d1  = OpReg W64 (RegReal (RealRegSingle 33))
d2  = OpReg W64 (RegReal (RealRegSingle 34))
d3  = OpReg W64 (RegReal (RealRegSingle 35))
d4  = OpReg W64 (RegReal (RealRegSingle 36))
d5  = OpReg W64 (RegReal (RealRegSingle 37))
d6  = OpReg W64 (RegReal (RealRegSingle 38))
d7  = OpReg W64 (RegReal (RealRegSingle 39))
d8  = OpReg W64 (RegReal (RealRegSingle 40))
d9  = OpReg W64 (RegReal (RealRegSingle 41))
d10 = OpReg W64 (RegReal (RealRegSingle 42))
d11 = OpReg W64 (RegReal (RealRegSingle 43))
d12 = OpReg W64 (RegReal (RealRegSingle 44))
d13 = OpReg W64 (RegReal (RealRegSingle 45))
d14 = OpReg W64 (RegReal (RealRegSingle 46))
d15 = OpReg W64 (RegReal (RealRegSingle 47))
d16 = OpReg W64 (RegReal (RealRegSingle 48))
d17 = OpReg W64 (RegReal (RealRegSingle 49))
d18 = OpReg W64 (RegReal (RealRegSingle 50))
d19 = OpReg W64 (RegReal (RealRegSingle 51))
d20 = OpReg W64 (RegReal (RealRegSingle 52))
d21 = OpReg W64 (RegReal (RealRegSingle 53))
d22 = OpReg W64 (RegReal (RealRegSingle 54))
d23 = OpReg W64 (RegReal (RealRegSingle 55))
d24 = OpReg W64 (RegReal (RealRegSingle 56))
d25 = OpReg W64 (RegReal (RealRegSingle 57))
d26 = OpReg W64 (RegReal (RealRegSingle 58))
d27 = OpReg W64 (RegReal (RealRegSingle 59))
d28 = OpReg W64 (RegReal (RealRegSingle 60))
d29 = OpReg W64 (RegReal (RealRegSingle 61))
d30 = OpReg W64 (RegReal (RealRegSingle 62))
d31 = OpReg W64 (RegReal (RealRegSingle 63))

opRegSExt :: Width -> Reg -> Operand
opRegSExt W64 r = OpRegExt W64 r ESXTX 0
opRegSExt W32 r = OpRegExt W32 r ESXTW 0
opRegSExt W16 r = OpRegExt W16 r ESXTH 0
opRegSExt W8  r = OpRegExt W8  r ESXTB 0
opRegSExt w  _r = pprPanic "opRegSExt" (ppr w)

fitsIn12bitImm :: (Num a, Ord a) => a -> Bool
fitsIn12bitImm off = off >= intMin12bit && off <= intMax12bit

intMin12bit :: Num a => a
intMin12bit = -2048

intMax12bit :: Num a => a
intMax12bit = 2047

fitsIn32bits  :: (Num a, Ord a, Bits a) => a -> Bool
fitsIn32bits i = (-1 `shiftL` 31) <= i && i <= (1 `shiftL` 31 -1)

