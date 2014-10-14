import Control.Monad (forM_, liftM, unless, when)
import Data.Maybe (isJust)
import qualified Data.Vector.Storable as VS
import Vision.Image (Grey, GreyPixel, InterpolMethod(..), convert, load,
                     manifestVector, resize, shape)
import Vision.Primitive ((:.)(..), ix2, Z(..))
import System.Console.Terminal.Size (size, height, width, Window(..))
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), getOpt,
                              OptDescr(..), usageInfo)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith, exitSuccess)
import System.IO (hPutStr, hPutStrLn, stderr)

import qualified System.Drawille as D

data Options = Options { optRatio     :: Maybe Double
                       , optThreshold :: Int
                       , optInvert    :: Bool
                       , optHelp      :: Bool
                       }
  deriving(Eq)

defaultOpts :: Options
defaultOpts = Options { optRatio     = Nothing
                      , optThreshold = 128
                      , optInvert    = False
                      , optHelp      = False
                      }


options :: [ OptDescr (Options -> Options) ]
options = [ Option "r" ["ratio"]
                   (ReqArg (\arg opt -> opt { optRatio = Just $ read arg })
                           "ratio")
                   "Image resize ratio"
          , Option "t" ["threshold"]
                   (ReqArg (\arg opt -> opt { optThreshold = read arg })
                           "threshold")
                   "Pixel printing threshold"
          , Option "i" ["invert"]
                   (NoArg (\opt -> opt { optInvert = True}))
                   "Invert colors"
          , Option "h" ["help"]
                   (NoArg (\opt -> opt { optHelp = True }))
                   "Display this message"
          ]

main :: IO ()
main = do
    (genOpts, files, errs) <- liftM (getOpt Permute options) getArgs
    let opts = foldr ($) defaultOpts genOpts

    when (not (null errs) || null files) $
        hPutStr stderr "\n" >>
        mapM_ (hPutStrLn stderr) errs >>
        printUsage >>
        exitWith (ExitFailure 1)

    when (optHelp opts) $
        hPutStr stderr "\n" >>
        printUsage >>
        exitSuccess

    maybeWindow <- size
    unless (isJust maybeWindow || isJust (optRatio opts)) $
        hPutStrLn stderr "Couldn't get window size and no ratio was provided" >>
        exitWith (ExitFailure 2)

    forM_ files $ \fp -> do
        errOrImage  <- load Nothing fp
        case errOrImage of
            Left err ->
                hPutStr stderr (show err) >>
                exitWith (ExitFailure 3)
            Right image ->
                let grey = convert image :: Grey
                    nwindow = getNWindow grey (optRatio opts) maybeWindow in
                    putStr $ D.frame $ fromImage nwindow
                                                 grey
                                                 (optThreshold opts)
                                                 (optInvert opts)

printUsage :: IO ()
printUsage = do
    name <- getProgName
    hPutStrLn stderr $ usageInfo ("Usage: " ++ name ++ " [options] files...")
                                 options

-- |
-- Gets a normalized Window given an image, a Maybe ratio and a Maybe
-- window.
getNWindow :: Grey -> Maybe Double -> Maybe (Window Int) -> Window Int
-- This is the base case. We'll want to resize the window to fit the
-- terminal's width and have its height scaled relative to that.
getNWindow image Nothing (Just window) = window { width = wwid
                                                , height = round whei
                                                }
  where Z :. h :. w = shape image
        wwid = 2 * width window
        whei = fromIntegral h * (fromIntegral wwid / fromIntegral w) :: Double
-- This is the ratio case, in which a factor to scale the image by is
-- given.
getNWindow image (Just ratio) _  =
    Window { height = round $ ratio * fromIntegral h
           , width  = round $ ratio * fromIntegral w
           }
  where Z :. h :. w = shape image
-- Because we check this is main, this pattern shouldn't ever run. It's
-- expected that one value or the other is provided - I think it'd be more
-- idiomatic to do this in two separate functions.
getNWindow _ Nothing Nothing = error "This shouldn't have happened"

-- |
-- Creates a canvas from a greyscale image.
fromImage :: Window Int -> Grey -> Int -> Bool -> D.Canvas
fromImage window greyImage threshold invert = VS.ifoldr accCanvas' D.empty imageVector
  where resizedImage = resizeToFitWindow window greyImage
        imageVector = manifestVector resizedImage
        {-# INLINEABLE accCanvas' #-}
        accCanvas' = accCanvas threshold (width window) invert

-- |
-- Gets a canvas coordinate given a canvas' width and a index to the flat
-- vector representation of an image's pixel matrix.
{-# INLINEABLE coord #-}
coord :: Int -> Int -> (Int, Int)
coord w i = (i `rem` w, w - i `div` w)

-- |
-- Accumulates pixels into a canvas.
{-# INLINEABLE accCanvas #-}
accCanvas :: Int -> Int -> Bool -> Int -> GreyPixel -> D.Canvas -> D.Canvas
accCanvas threshold winW invert idx pix m =
    let withinThreshold = pix < fromIntegral threshold
        shouldSet = if invert then not withinThreshold else withinThreshold
      in if shouldSet
             then D.set m (coord winW idx)
             else m

-- |
-- Resizes an image to fit a window.
resizeToFitWindow :: Window Int -> Grey -> Grey
resizeToFitWindow win = resize TruncateInteger (ix2 h w)
  where h = height win
        w = width win
