import Control.Monad (liftM)
import qualified Data.Vector.Storable as VS
import Vision.Image (Grey, GreyPixel, InterpolMethod(..), convert, load,
                     manifestVector, resize)
import Vision.Primitive (ix2)
import Vision.Image.Storage (StorageImage)
import System.Console.Terminal.Size
import System.Environment (getArgs)

import qualified System.Drawille as D

main :: IO ()
main = do
    pathToImage <- liftM head getArgs
    maybeWindow <- size
    errOrImage  <- load Nothing pathToImage

    case maybeWindow of
        Just window -> do
            let nwindow = window { height = 4 * height window
                                 , width  = 2 * width window
                                 }
            case errOrImage of
                Left err ->
                    putStr $ show err
                Right image ->
                    putStr $ D.frame $ fromImage nwindow image 128
        Nothing ->
            putStr "Unable to get window size"

fromImage :: Window Int -> StorageImage -> Int -> D.Canvas
fromImage window image threshold = VS.ifoldr accCanvas' D.empty imageVector
  where greyImage = convert image :: Grey
        resizedImage = resizeToFitWindow window greyImage
        imageVector = manifestVector resizedImage
        {-# INLINEABLE accCanvas' #-}
        accCanvas' = accCanvas threshold $ width window

{-# INLINEABLE coord #-}
coord :: Int -> Int -> (Int, Int)
coord w i = (i `rem` w, w - i `div` w)

{-# INLINEABLE accCanvas #-}
accCanvas :: Int -> Int -> Int -> GreyPixel -> D.Canvas -> D.Canvas
accCanvas threshold winW idx pix m = if pix < fromIntegral threshold
                                    then D.set m (coord winW idx)
                                    else m

resizeToFitWindow :: Window Int -> Grey -> Grey
resizeToFitWindow win = resize TruncateInteger (ix2 h w)
  where h = height win
        w = width win
