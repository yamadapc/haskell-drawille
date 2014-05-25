module System.Drawille ( empty -- drawing API
                       , frame
                       , get
                       , set
                       , unset
                       , toggle
                       , fromList

                       , toPs -- utility functions
                       , toPx
                       , pxMap
                       , pxOff
                       ) where

import qualified Data.Map as M (Map, empty, lookup, insertWith, keys)
import Data.Bits ((.|.), (.&.), complement, xor)
import Data.Char (chr)

type Canvas = M.Map (Int, Int) Int

pxMap :: Num a => [[a]]
pxMap = [ [0x01, 0x08]
        , [0x02, 0x10]
        , [0x04, 0x20]
        , [0x40, 0x80]
        ]

pxOff :: Num a => a
pxOff = 0x2800

toPx :: (Int, Int) -> Int
toPx (px, py) = pxMap !! mod py 4 !! mod px 2

toPs :: (Int, Int) -> (Int, Int)
toPs (x, y) = (x `div` 2, y `div` 4)

empty :: Canvas
empty = M.empty

frame :: Canvas -> String
frame c = unlines $ map (row c mX) [minY..maxY]
    where keys = M.keys c
          xs = map fst keys
          ys = map snd keys
          mX = maximumMinimum xs
          (maxY, minY) = maximumMinimum ys

row :: Canvas -> (Int, Int) -> Int -> String
row c (maxX, minX) y = map helper vs
    where vs = map (\x -> M.lookup (x, y) c) [minX..maxX]
          helper (Just v) = chr $ v + pxOff
          helper Nothing  = ' '

set :: Canvas -> (Int, Int) -> Canvas
set c p = M.insertWith (.|.) (toPs p) (toPx p) c

get :: Canvas -> (Int, Int) -> Bool
get c p = case M.lookup (toPs p) c of
            Just x  -> let px = toPx p in x .&. px == px
            Nothing -> False

unset :: Canvas -> (Int, Int) -> Canvas
unset c p = M.insertWith (.&.) (toPs p) ((complement . toPx) p) c

toggle :: Canvas -> (Int, Int) -> Canvas
toggle c p = M.insertWith xor (toPs p) (toPx p) c

fromList :: [(Int, Int)] -> Canvas
fromList = foldr (flip set) empty

maximumMinimum :: Ord a => [a] -> (a, a)
maximumMinimum (x:xs) = foldr maxMin (x, x) xs
maximumMinimum [] = error "Empty list"

maxMin :: Ord a => a -> (a, a) -> (a, a)
maxMin x (b, s) = (max x b, min x s)
