import Control.Monad (liftM)
import qualified Data.Vector.Storable as VS
import Vision.Image (Grey, InterpolMethod(..), convert, load, manifestVector,
                     resize)
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
fromImage window image threshold = D.fromList $
                                   VS.ifoldr helper [] imageVector

  where greyImage = convert image :: Grey
        resizedImage = resizeToFitWindow window greyImage
        imageVector = manifestVector resizedImage

        coord i = (i `rem` w, i `div` w)
          where w = width window

        helper idx pix m = if pix < fromIntegral threshold
                               then coord idx : m
                               else m

resizeToFitWindow :: Window Int -> Grey -> Grey
resizeToFitWindow win = resize Bilinear (ix2 h w)
  where h = height win
        w = width win
