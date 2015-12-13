module Assignment where

import Dist

unwrap :: Dist Double -> [(t, Double)]
unwrap (Dist a) = a

append :: [(a, Double)] -> Dist a -> Dist a
append xs pxs = Dist (upxs ++ xs)
  where
    upxs = unwrap pxs

mean  :: Dist Double -> Double
mean pxs = (fst upxs ) * (snd upxs)
 where
   upxs = unwrap pxs

{-|
  We map our computeWin . computeLoss across our distribution
  This effectively doubles our Dist everytime we call it
  How do we map it 10 times
-}

dreidelDreidelDreidel :: Double -> Double -> Int -> Dist Double
dreidelDreidelDreidel y0 p n = dreidelDreidelDreidel' p n-1
                                Dist ([(y0 + 10 * p * (y0), 1/4), (y0 - 10 * p * (y0), 3/4) ])

dreidelDreidelDreidel' :: Double -> Int -> Dist Double -> Dist Double
dreidelDreidelDreidel' p n pxs = map (computeWin p n . computeLoss p n) pxs >>= dreidelDreidelDreidel' p n-1
dreidelDreidelDreidel' p 0 pxs = map (computeWin p n . computeLoss p n) pxs >>= return

computeWin :: Double -> Double -> Dist Double -> Dist Double
computeWin y0 p currentDist = do
  currentPot <- fst . unwrap currentDist
  currentProbability <- snd . unwrap currentDist
  ((currentPot + (10 * p) *(currentPot)), (currentProbability * 1/4))

computeLoss :: Double -> Double -> Dist Double -> Dist Double
computeLoss y0 p currentDist = do
  currentPot <- fst . unwrap currentDist
  currentProbability <- snd . unwrap currentDist
  ((currentPot - (10 * p) *(currentPot)), (currentProbability * 3/4))

main :: IO ()
main = print $ dreidelDreidelDreidel 1000 0.5 10
