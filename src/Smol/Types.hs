module Smol.Types
    ( SmolType(..)
    , isSubtypeOf
    , (<:)
    , (>:)
    , voidType
    , unitType
    , getCardinality
    , isInhabited
    , createArrow
    , createTuple
    ) where

import Smol.Types.Cardinality (Cardinality (..), power)


data SmolType
    = TopType
    | SumType [SmolType] -- disjoint unions
    | ProductType [SmolType] -- tuples
    | ArrowType { argType :: SmolType, returnType :: SmolType } -- functions
    | EnumType { enumMembersCount :: Integer, nominalId :: Int } -- simple enumerations
    deriving (Eq, Show)


voidType :: SmolType
voidType = SumType []
unitType :: SmolType
unitType = ProductType []


isSubtypeOf :: SmolType -> SmolType -> Bool
isSubtypeOf TopType _ = False
isSubtypeOf _ TopType = True

-- TODO: This may actually be not handling all the cases
isSubtypeOf (SumType as) (SumType bs) = all (\a -> any (a <:) bs) as
isSubtypeOf (SumType ts) x = all (<: x) ts
isSubtypeOf x (SumType ts) = any (x <:) ts

isSubtypeOf (ArrowType a1 r1) (ArrowType a2 r2) = a2 <: a1 && r1 <: r2

isSubtypeOf (ProductType as) (ProductType bs) = all (uncurry (<:)) $ zip as bs

isSubtypeOf _ _ = False

infix 4 <:
(<:) :: SmolType -> SmolType -> Bool
(<:) = isSubtypeOf

infix 4 >:
(>:) :: SmolType -> SmolType -> Bool
(>:) = flip isSubtypeOf


-- Retrieves an amount of possible values of the type
getCardinality :: SmolType -> Cardinality
getCardinality TopType = Uncountable
getCardinality (SumType ts) = foldl (+) (Finite 0) (map getCardinality ts) -- union is a sum
getCardinality (ProductType ts) = foldl (*) (Finite 1) (map getCardinality ts) -- product is a, well, product
getCardinality (ArrowType a b) = getCardinality b `power` getCardinality a -- arrow is an inverse power
getCardinality (EnumType count _) = Finite count


-- Checks if there are any valid values inhabiting the type
isInhabited :: SmolType -> Bool
isInhabited t =
    case getCardinality t of
        (Finite 0) -> False
        _ -> True

-- Concatenates tuple types (other types are converted to a tuple)
createTuple :: SmolType -> SmolType -> SmolType
createTuple (ProductType as) (ProductType bs) = ProductType (as ++ bs)
createTuple t u = ProductType [t, u]


-- Creates an arrow type for inhabited return type, or returns void type otherwise
createArrow :: SmolType -> SmolType -> SmolType
createArrow a r =
    if isInhabited r then
        ArrowType a r
    else
        voidType


