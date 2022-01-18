module Smol.Types.Cardinality
    ( Cardinality(..)
    , power
    , add
    , multiply
    ) where
import GHC.Float.RealFracMethods (ceilingFloatInteger)


data Cardinality
    = Finite Integer
    | Countable
    | Uncountable
    deriving (Eq, Show)

instance Ord Cardinality where
    compare (Finite a) (Finite b) = compare a b
    compare (Finite _) _ = LT

    compare Countable (Finite _) = GT
    compare Countable Countable = EQ
    compare Countable Uncountable = LT

    compare Uncountable Uncountable = EQ
    compare Uncountable _ = GT

instance Num Cardinality where
    (+) = add
    (*) = multiply
    abs = id
    fromInteger = Finite

    signum (Finite 0) = Finite 0
    signum _ = Finite 1

    negate _ = Finite 0

add :: Cardinality -> Cardinality -> Cardinality
add (Finite a) (Finite b) = Finite (a + b)
add (Finite _) Countable = Countable
add Countable (Finite _) = Countable
add Countable Countable = Countable
add _ _ = Uncountable

multiply :: Cardinality -> Cardinality -> Cardinality
multiply (Finite a) (Finite b) = Finite (a * b)
multiply (Finite _) Countable = Countable
multiply Countable (Finite _) = Countable
multiply Countable Countable = Countable
multiply _ _ = Uncountable

power :: Cardinality -> Cardinality -> Cardinality
power (Finite a) (Finite b) = Finite (a ^ b)
power x (Finite _) = x
power _ _ = Uncountable

log2 :: Cardinality -> Maybe Integer
log2 (Finite 0) = Just 0
log2 (Finite 1) = Just 0
log2 (Finite n) = Just $ ceilingFloatInteger (logBase 2.0 (fromInteger n :: Float))
log2 _ = Nothing
