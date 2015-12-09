module Assignment where

import Dist

unwrap :: Dist t -> [(t, Double)]
unwrap (Dist a) = a

append :: [(a, Double)] -> Dist a -> Dist a
append xs pxs = return Dist (upxs ++ xs)
  where
    upxs = unwrap pxs

mean  :: Dist xs -> Double
mean pxs = (sum (map snd upxs) / (realToFrac $ length upxs))
 where
   upxs = unwrap pxs

dreidel = Dist [("Win", 0.25), ("Lose", 0.75)]

dreidelDreidelDreidel :: Double -> Double -> Int -> Dist Double
dreidelDreidelDreidel y0 p n = append (result) dreidelDreidelDreidel y1 p n+1
  where
    result = dreidelProbT y0 p n
    y1 = addOrRemove y0
dreidelDreidelDreidel y0 p 1 = getProbOfWinning

dreidelProbT :: Double -> Double -> Int -> Dist Double
dreidelProbT y0 p n = undefined

dreidelToDistString :: String -> Dist "String"
dreidelToDistString "Nun" = Dist [("Lose", 1/4)]
dreidelToDistString "Gimel" = Dist [("Win", 1/4)]
dreidelToDistString "He" = Dist [("Lose", 1/4)]
dreidelToDistString "Shin" = Dist [("Lose", 1/4)]

main :: IO ()
main = mapM_ putStrLn . dreidelDreidelDreidel 1000 0.5 10
