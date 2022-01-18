module Smol.Types.Operations
    ( getUnion
    , getIntersection
    ) where

import Smol.Types (SmolType(..), isInhabited, voidType)

-- Finds a simpliest union for 2 types
getUnion :: SmolType -> SmolType -> SmolType

getUnion TopType _ = TopType
getUnion _ TopType = TopType

-- TODO: figure out what should happen if lengths differ
getUnion (ProductType as) (ProductType bs) =
    if length as == length bs then
        productOfUnions
    else
        SumType [ProductType as, ProductType bs]
    where
        productOfUnions :: SmolType
        productOfUnions = ProductType $ zipWith getUnion as bs

getUnion (ArrowType a1 r1) (ArrowType a2 r2) = ArrowType a r
    where
        a = getIntersection a1 a2
        r = getUnion r1 r2

getUnion (SumType []) t = t
getUnion t (SumType []) = t

getUnion (SumType as) (SumType bs) = foldl getUnion voidType (as ++ bs)

getUnion a b = SumType [a, b]

-- Finds a simpliest intersection for 2 types
getIntersection :: SmolType -> SmolType -> SmolType

getIntersection TopType t = t
getIntersection t TopType = t

getIntersection (SumType as) (SumType bs) = foldl getUnion voidType combinations where
    combinations :: [SmolType]
    combinations = do
        a <- as
        getIntersection a <$> bs

getIntersection (ArrowType a1 r1) (ArrowType a2 r2) =
    if isInhabited r then
        ArrowType a r
    else
        voidType
    where
        a = getUnion a1 a2
        r = getIntersection r1 r2

-- TODO: figure out what should happen if lengths differ
getIntersection (ProductType as) (ProductType bs) =
    if length as == length bs then
        ProductType $ zipWith getIntersection as bs
    else
        voidType

getIntersection t u = if t == u then t else voidType
