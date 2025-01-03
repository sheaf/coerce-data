{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=CoerceData.Plugin #-}
{-# OPTIONS_GHC -dcore-lint #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

--{-# OPTIONS_GHC -ddump-tc-trace -ddump-to-file #-}

module Tests where

-- base
import Data.Coerce
  ( coerce )
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(..) )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Data.Typeable
  ( Typeable, eqT )

--------------------------------------------------------------------------------
-- Simple datatypes

data A1 = A1 Int
  deriving stock ( Eq, Show )
data A2 = A2 Int
  deriving stock ( Eq, Show )

a1 :: A1
a1 = A1 7

a2 :: A2
a2 = A2 7

ca1 :: A1
ca1 = coerce a2

ca2 :: A2
ca2 = coerce a1

--------------------------------------------------------------------------------
-- Slightly more involved, yet still vanilla, datatypes

data B1 a b = B1 a Char a b
  deriving stock ( Eq, Show )
data B2 x y = B2 Int x y Bool
  deriving stock ( Eq, Show )

b1 :: B1 Int Bool
b1 = B1 7 'b' 99 True

b2 :: B2 Char Int
b2 = B2 7 'b' 99 True

cb1 :: B1 Int Bool
cb1 = coerce b2

cb2 :: B2 Char Int
cb2 = coerce b1

--------------------------------------------------------------------------------
-- Coercing between types and constraints

class Cls a where
  methC :: a
instance Cls Int where
  methC = 17

type C1 :: Type -> Type
data C1 a = C1 a
  deriving stock ( Eq, Show )

type C2 :: Type -> Type
data C2 a where
  C2 :: Cls a => C2 a

instance ( Cls a, Show a ) => Show ( C2 a ) where
  show ( C2 @a ) = show ( methC @a )
instance ( Cls a, Eq a ) => Eq ( C2 a ) where
  C2 @a1 == C2 @a2 = methC @a1 == methC @a2

c1 :: C1 Int
c1 = C1 17

c2 :: C2 Int
c2 = C2

cc1 :: C1 Int
cc1 = coerce c2

cc2 :: C2 Int
cc2 = coerce c1

--------------------------------------------------------------------------------
-- GADTs and existentials

type E1 :: Type -> Type
data E1 a where
  E1 :: forall a b c. ( Typeable c, Eq c, Show c ) => Proxy b -> ( c, Bool ) -> a -> E1 a

instance Show a => Show ( E1 a ) where
  show ( E1 _ c a ) = show ( c, a )
instance Eq a => Eq ( E1 a ) where
  E1 _ ( c1 :: c1 ) a1 == E1 _ ( c2 :: c2 ) a2
    | Just Refl <- eqT @c1 @c2
    = c1 == c2 && a1 == a2
    | otherwise
    = False

type E2 :: Type -> Type -> Type
data E2 a z where
  E2 :: forall a b c z. ( Typeable b, Eq b, Show b ) => Proxy c -> ( b, z ) -> a -> E2 a z

instance ( Show a, Show z ) => Show ( E2 a z ) where
  show ( E2 _ b a ) = show ( b, a )
instance ( Eq a, Eq z ) => Eq ( E2 a z ) where
  E2 _ ( b1 :: b1, z1 ) a1 == E2 _ ( b2 :: b2, z2 ) a2
    | Just Refl <- eqT @b1 @b2
    = b1 == b2 && a1 == a2 && z1 == z2
    | otherwise
    = False

e1 :: E1 Char
e1 = E1 @Char @Float @Int Proxy ( 3, False ) 'x'

e2 :: E2 Char Bool
e2 = E2 @Char @Int @Double @Bool Proxy ( 3, False ) 'x'

ce1 :: E1 Char
ce1 = coerce e2

ce2 :: E2 Char Bool
ce2 = coerce e1

--------------------------------------------------------------------------------
-- Data families

type F1 :: Type -> Type -> Type
data family F1 a b

data instance F1 w Char = F1 !w Char
instance Show w => Show ( F1 w Char ) where
  show ( F1 w c ) = show ( w, c )
instance Eq w => Eq ( F1 w Char ) where
  F1 w1 c1 == F1 w2 c2 = w1 == w2 && c1 == c2

type F2 :: Type -> Type
data F2 w = F2 !w Char
  deriving stock ( Eq, Show )

f1 :: F1 Word Char
f1 = F1 3 'c'

f2 :: F2 Word
f2 = F2 3 'c'

cf1 :: F1 Word Char
cf1 = coerce f2

cf2 :: F2 Word
cf2 = coerce f1

--------------------------------------------------------------------------------