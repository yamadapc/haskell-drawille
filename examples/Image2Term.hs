import Control.Monad (liftM)
import Vision.Image (Grey, GreyPixel, InterpolMethod(..), convert, load, resize)
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
        Just window ->
            case errOrImage of
                Left err ->
                    putStr $ show err
                Right image ->
                    putStr $ D.frame $ fromImage window image 128
        Nothing ->
            putStr ""

fromImage :: Window Int -> StorageImage -> Int -> D.Canvas
fromImage window image threshold = undefined -- D.fromList $ map pixToTuple resizedImage
  where greyImage = convert image :: Grey
        resizedImage = resizeToFitWindow window greyImage
        pixToTuple :: GreyPixel -> (Int, Int)
        pixToTuple = undefined

resizeToFitWindow :: Window Int -> Grey -> Grey
resizeToFitWindow win = resize Bilinear (ix2 h w)
  where h = height win
        w = width win
