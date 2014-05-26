import Data.Angle
import Data.List (union)
import Control.Arrow ((&&&))
import qualified System.Drawille as D

main :: IO ()
main = do
    let s = D.fromList $ take 1800 senoid
    putStr $ D.frame s

    let cs = D.fromList $ union (take 1800 senoid) (take 1800 cosenoid)
    putStr $ D.frame cs

senoid :: Integral a => [(a, a)]
senoid = map ((`div` 10) &&& (floor . (* 10) . rsine . fromIntegral)) [0..]

cosenoid :: Integral a => [(a, a)]
cosenoid = map ((`div` 10) &&& (floor . (* 10) . rcosine . fromIntegral)) [0..]

rsine :: Double -> Double
rsine = sine . radians . Degrees

rcosine :: Double -> Double
rcosine = cosine . radians . Degrees
