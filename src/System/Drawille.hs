-- |
-- Module      :  System.Drawille
-- Description :  A port of asciimoo's drawille to haskell.
-- Copyright   :  (c) Pedro Yamada
-- License     :  GPL-3
--
-- Maintainer  :  Pedro Yamada <tacla.yamada@gmail.com>
-- Stability   :  stable
-- Portability :  non-portable (not tested on multiple environments)
--
-- This module enables using UTF-8 braille characters to render drawings onto
-- the console.
module System.Drawille ( Canvas
                       , empty -- drawing API
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

-- |
-- The Canvas type. Represents a canvas, mapping points to their braille
-- "px" codes.
type Canvas = M.Map (Int, Int) Int

-- |
-- The empty canvas, to be drawn upon.
empty :: Canvas
empty = M.empty

-- |
-- Pretty prints a canvas as a `String`, ready to be printed.
frame :: Canvas -> String
frame c = unlines $ map (row c mX) [minY..maxY]
    where keys = M.keys c
          mX = maximumMinimum $ map fst keys
          (maxY, minY) = maximumMinimum $ map snd keys

-- |
-- Gets the current state for a coordinate in a canvas.
get :: Canvas -> (Int, Int) -> Bool
get c p = case M.lookup (toPs p) c of
            Just x  -> let px = toPx p in x .&. px == px
            Nothing -> False

-- |
-- Sets a coordinate in a canvas.
set :: Canvas -> (Int, Int) -> Canvas
set c p = M.insertWith (.|.) (toPs p) (toPx p) c

-- |
-- Unsets a coordinate in a canvas.
unset :: Canvas -> (Int, Int) -> Canvas
unset c p = M.insertWith (.&.) (toPs p) ((complement . toPx) p) c

-- |
-- Toggles the state of a coordinate in a canvas
toggle :: Canvas -> (Int, Int) -> Canvas
toggle c p = M.insertWith xor (toPs p) (toPx p) c

-- |
-- Creates a canvas from a List of coordinates
fromList :: [(Int, Int)] -> Canvas
fromList = foldr (flip set) empty

-- |
-- Pretty prints a single canvas' row into a `String`.
row :: Canvas -> (Int, Int) -> Int -> String
row c (maxX, minX) y = map helper vs
    where vs = map (\x -> M.lookup (x, y) c) [minX..maxX]
          helper (Just v) = chr $ v + pxOff
          helper Nothing  = ' '

-- |
-- A mapping between local coordinates, inside of a single cell, and each
-- of the braille characters they correspond to (with an offset).
pxMap :: Num a => [[a]]
pxMap = [ [0x01, 0x08]
        , [0x02, 0x10]
        , [0x04, 0x20]
        , [0x40, 0x80]
        ]

-- |
-- The offset between the values in the `pxMap`, which have nice binary
-- properties between each other, and the actual braille character codes.
pxOff :: Num a => a
pxOff = 0x2800

-- |
-- Converts a coordinate into its local braille "px" code, using the
-- `pxMap`.
toPx :: (Int, Int) -> Int
toPx (px, py) = pxMap !! mod py 4 !! mod px 2

-- |
-- Helper to convert a coordinate to its corespondent in the bigger braille
-- grid's size
toPs :: (Int, Int) -> (Int, Int)
toPs (x, y) = (x `div` 2, y `div` 4)

-- |
-- Gets the maximum and minimum values of a list and return them in
-- a tuple of `(maximumValue, minimumValue)`.
maximumMinimum :: Ord a => [a] -> (a, a)
maximumMinimum [] = error "Empty list"
maximumMinimum (x:xs) = foldr maxMin (x, x) xs
  where maxMin y (b, s) = (max y b, min y s)
