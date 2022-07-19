module OptTh.Examples.Comonad.HarborProblem where

import Prelude
import GHC.Generics (Generic)
import Control.Comonad.Env
import Control.Comonad.Store
import Data.Default

type Container = Int
type Crane = Int

data Harbor = Harbor { _b :: Int
                     , _s :: Int
                     , _c :: Double
                     , _l :: Container -> Int
                     , _o :: Crane -> Container -> Double
                     , _jMax :: Container
                     , _kMax :: Crane
                     } deriving (Generic)

data Pos = Pos { _j :: Container
               , _k :: Crane
               , _r :: Int
               } deriving (Generic)

instance Semigroup Harbor where a <> _ = a

instance Semigroup Pos where a <> b = Pos (_j a + _j b) (_k a + _k b) (_r a + _r b)
instance Default   Pos where def = Pos 0 0 0
instance Monoid    Pos where mempty = def

-- Comonads.

type PStore = Store Pos

type P = EnvT Harbor PStore

δ :: P a -> Int
δ p = _r (pos p) - _b (ask p)

rowCost :: P a -> Double
rowCost p = _c (ask p) * (fromIntegral (δ p) ** 2)

pL :: P a -> Int
pL p = _l (ask p) (_j (pos p))

placable :: P a -> Bool
placable p = δ p >= pL p

cost :: P Double -> Double
cost p | j == jMax = extract p -- Nothing to do/add when there are no more containers.
       | otherwise = minimum possibilities -- Do the next step.
  where
    j         = _j    (pos p)
    k         = _k    (pos p)
    r         = _r    (pos p)
    b         = _b    (ask p)
    s         = _s    (ask p)
    c         = _c    (ask p)
    l         = _l    (ask p) j
    jMax      = _jMax (ask p)
    kMax      = _kMax (ask p)
    δ         = r - b
    placeCost = _o    (ask p) k j
    rowCost   = c * (fromIntegral δ ** 2)
    placeable = δ >= l + s

    nextRow   = peeks (\q -> q{_r = 0        }) p + rowCost
    nextCrane = peeks (\q -> q{_r = 0        , _k = k + 1}) p + rowCost
    place     = peeks (\q -> q{_r = r + s + l, _j = j + 1}) p + placeCost

    possibilities | placeable = [nextRow, nextCrane, place]
                  | otherwise = [nextRow, nextCrane]


-- Solution info.

type Solution = [[[Container]]]

rowδ' :: Harbor -> [Container] -> Int
rowδ' h js = sum (_l h <$> js)

rowCost' :: Harbor -> [Container] -> Double
rowCost' h js = _c h * (fromIntegral (rowδ' h js) ** 2)

oRow' :: Harbor -> Crane -> [Container] -> Double
oRow' h k js = sum (_o h k <$> js)

totRow' :: Harbor -> Crane -> [Container] -> Double
totRow' h k js = oRow' h k js + rowCost' h js

totCrane' :: Harbor -> Crane -> [[Container]] -> Double
totCrane' h k jss = sum (totRow' h k <$> jss)

tot' :: Harbor -> [[[Container]]] -> Double
tot' h jss = sum ( zipWith (totCrane' h) [0.._kMax h - 1] jss)