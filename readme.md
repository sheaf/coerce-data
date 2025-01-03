# coerce-data <a href="https://hackage.haskell.org/package/coerce-data" alt="Hackage"><img src="https://img.shields.io/hackage/v/coerce-data.svg" /></a>

What if you could extend Haskell's iconic `coerce` function to work on data
types that share a structure — not just newtypes? That's exactly what
**coerce-data** brings to the table: safe, zero-cost coercions between
data types with isomorphic data constructors, powered by a GHC type-checking
plugin.

# Example

Example:

```hs
data D1 = MkD1 Int !Bool
data D2 a = MkD2 a !Bool

oneTwo :: [D1] -> [D2 Int]
oneTwo = coerce
```

With **coerce-data**, this works seamlessly. No hacks, no `unsafeCoerce`, just
type-safe, runtime-free elegance.

# Motivation

Haskell's `coerce` function is a cornerstone of zero-cost abstractions,
allowing safe and efficient transformations between types.

However, there's a catch: it only works with newtypes. This means many seemingly
obvious conversions are off-limits, like converting between structurally
identical types with different names.

Until now, the workaround has often been to reach for `unsafeCoerce`, but let's
be honest: nobody likes debugging segfaults caused by a stray change in the
strictness of a data constructor argument in some distant library.  
With **coerce-data**, you can leave those worries behind.

# Usage

To use **coerce-data**:

  - Add `coerce-data` to the `build-depends` stanza in your `.cabal` file.
  - Enable the plugin in any module where you'd like to use it:

```hs
{-# OPTIONS_GHC -fplugin=CoerceData.Plugin #-}
```

# Use-cases

## Length-indexed vectors

I often find myself making use of length-indexed vectors, in particular
with the following definition:

```hs
type Vec :: Nat -> Type -> Type
data Vec n a where
  VNil  :: Vec 0
  (:::) :: a -> Vec n a -> Vec (1 + n) a
```

Note that this data type has the **exact** same structure as the list type,
because the length argument is erased at runtime. With **coerce-data**, one can
immediately derive useful instances for this type, e.g.:

```hs
deriving via List    instance Functor     ( Vec n )
deriving via List    instance Foldable    ( Vec n )
deriving via List    instance Traversable ( Vec n )
deriving via ZipList instance Applicative ( Vec n )
```

## Safely poking into a library's internals

Sometimes, you need to work with a library's internals, but patching the library
to expose what you need — whether through public constructors or a `.Internal`
module — just isn't an option. Perhaps you're building your own library
and you don't want to force your users to upgrade their dependencies.

Consider this scenario: a `fancy-library` exposes a type but hides its
constructors:

```hs
-- in 'fancy-library'
data FancyType
  = InternalCon1 !Int
  | InternalCon2 Bool
```

With **coerce-data**, we can do the following:

```hs
-- in 'my-library'
data MyFancyType
  = MyInternalCon1 !Int
  | MyInternalCon2 Bool

peekAndPoke :: (FancyType -> FancyType) -> MyFancyType -> MyFancyType
peekAndPoke = coerce
```

If `FancyType` changes, this code won't compile until you update it,
ensuring it's easily maintainable: it doesn't make any unchecked assumptions
about the original type, unlike using `unsafeCoerce`.
