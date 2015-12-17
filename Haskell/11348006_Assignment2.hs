module Dreidel where
import Dist
import Data.Ord
import Data.List

dreidelDreidelDreidel :: Int -> Double -> Int -> Dist Double
dreidelDreidelDreidel pot p n = do
  dreidelDreidelDreidel' p n (distributionOfPot)
    where
      potToDouble = realToFrac pot
      distributionOfPot = return potToDouble :: Dist Double

dreidelDreidelDreidel' :: Double -> Int -> Dist Double -> Dist Double
dreidelDreidelDreidel' p 1 pxs = Dist ((computeWin pxs p) ++ (computeLoss pxs p))
dreidelDreidelDreidel' p n pxs = dreidelDreidelDreidel' p (n-1) npxs
  where
    npxs = Dist ((computeWin pxs p) ++ (computeLoss pxs p))

computeWin :: Dist Double -> Double -> [(Double, Double)]
computeWin (Dist pxs) portion = [(x + ((10 * portion) * x), p * 1/4)| (x,p) <- pxs ]

computeLoss :: Dist Double -> Double -> [(Double, Double)]
computeLoss (Dist pxs) portion = [(x - (portion * x), p * 3/4) | (x,p) <- pxs ]

mean :: Dist Double -> Double
mean (Dist pxs) = sum [(x*p) | (x,p) <- pxs]

-- Find p which maximizes mean (dreidelDreidelDreidel 1000 p 10)
-- 1.0
pWhereMaximizesPotentialReturn :: Double
pWhereMaximizesPotentialReturn = fst . maximumBy (comparing snd) $ zip [0.1, 0.2 .. 1.0]  [(mean $ (dreidelDreidelDreidel 1000 x 10)) | x <- [0.1,0.2..1.0]]

-- Find p which maximizes prExceeds 4000 (dreidelDreidelDreidel 1000 p 10))
-- 0.30000000000000004
maximumPortion :: Double
maximumPortion = fst . maximumBy (comparing snd) $ zip [0.1, 0.2 .. 1.0 ] [prExceeds 4000 $ dreidelDreidelDreidel 1000 p 10 | p <-[0.1, 0.2 .. 1.0 ]]

-- What is the probability of ending up with over 4000 using that value of p?
-- 0.4744071960449219
probabilityOfExceeding :: Double
probabilityOfExceeding = snd . maximumBy (comparing snd) $ zip [0.1, 0.2 .. 1.0 ] [prExceeds 4000 $ dreidelDreidelDreidel 1000 p 10 | p <-[0.1, 0.2 .. 1.0 ]]

-- What is the expected (i.e., mean) amount ended up with from this p?
-- 68029.68136433346
meanAmountFromMaximum :: Double
meanAmountFromMaximum = mean $ (dreidelDreidelDreidel 1000 maximumPortion 10)

prExceeds :: Double -> Dist Double -> Double
prExceeds target dist = mean $ fmap (fromIntegral . fromEnum . (>= target)) dist
