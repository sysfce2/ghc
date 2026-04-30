{- --------------------------------------------------------------------------
// Dirty CPP hackery for CTypes/CTypesISO
//
// (c) The FFI task force, 2000
// --------------------------------------------------------------------------
-}

#pragma once

--  // GHC can derive any class for a newtype, so we make use of that here...

#define ARITHMETIC_CLASSES  Eq,Ord,Num,Enum,Storable,Real
#define INTEGRAL_CLASSES Bounded,Integral,Bits,FiniteBits
#define FLOATING_CLASSES Fractional,Floating,RealFrac,RealFloat
#define OPAQUE_CLASSES Eq,Ord,Storable

#define ARITHMETIC_TYPE(T,THE_CTYPE,B) \
newtype {-# CTYPE THE_CTYPE #-} T = T B deriving newtype (Read, Show, ARITHMETIC_CLASSES);

#define INTEGRAL_TYPE(T,THE_CTYPE,B) \
newtype {-# CTYPE THE_CTYPE #-} T = T B \
    deriving newtype (Read, Show, ARITHMETIC_CLASSES, INTEGRAL_CLASSES, Ix);

#define FLOATING_TYPE(T,THE_CTYPE,B) \
newtype {-# CTYPE THE_CTYPE #-} T = T B deriving newtype (Read, Show, ARITHMETIC_CLASSES, FLOATING_CLASSES, Bounded);

#define FLOATING_TYPE_WITH_CTYPE(T,THE_CTYPE,B) \
newtype {-# CTYPE THE_CTYPE #-} T = T B \
    deriving newtype (Read, Show, ARITHMETIC_CLASSES, FLOATING_CLASSES);

#define OPAQUE_TYPE(T,THE_CTYPE,B) \
newtype {-# CTYPE THE_CTYPE #-} T = T (B) \
    deriving newtype (Show, OPAQUE_CLASSES);
