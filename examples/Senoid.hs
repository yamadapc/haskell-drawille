import Data.Angle
import qualified System.Drawille as D

main :: IO ()
main = do
    let c = D.fromList $ take 1800 senoid
    putStr $ D.frame c

senoid :: Integral a => [(a, a)]
senoid = zip (map (`div` 10) [0..]) (map (floor . (* 10) . rsine) [0..])

rsine :: Double -> Double
rsine = sine . radians . Degrees
