{-# LANGUAGE CPP                #-}

module Main where

import qualified Tiff
import qualified Png
import qualified Jpeg
import qualified Bmp
import qualified Gif

#ifdef COMPLETE
import qualified Zip
import qualified Tga
import qualified Ogg
import qualified Tar
import qualified Xml
import qualified Html
import qualified Pnm
import qualified Gzip
import qualified Bzip
import qualified Js
import qualified SimpleSvg
import qualified Svg
--import qualified MBox
import qualified Css
import qualified Dot
import qualified ByteString
import qualified TTF
#endif

import System.Console.ArgParser
import System.Random
import Args
import Data.Maybe
import System.Directory 
import System.Exit
import Control.Monad
import Data.List.Split

fillArgs :: MainArgs -> IO (String -> MainArgs)
fillArgs args =
    case findFileName args of
        [] -> do
            sG <- getStdGen
            let fname = take 10 ((randomRs ('a','z') sG) :: String )
            return $ formatArgs (formatFileName args fname)
        _ -> return $ formatArgs args

dispatch :: MainArgs -> IO ()
dispatch arg = do
        args <- fillArgs arg
        let b = findPar arg
        safetyChecks arg 
        case findFileType arg of
            "Bmp"  -> Bmp.main (args "")
            "Gif"  -> Gif.main args b
            "Jpeg" -> Jpeg.main (args "")
            "Png"  -> Png.main (args "")
            "Tiff" -> Tiff.main (args "")
#ifdef COMPLETE
            "Dot"  -> Dot.main (args "")
            "Ogg"  -> Ogg.main (args "")
            "Zip"  -> Zip.main (args "")
            "Bzip" -> Bzip.main (args "")
            "Gzip" -> Bzip.main (args "")
            "Tar"  -> Tar.main (args "")
            "Tga"  -> Tga.main (args "")
            "Xml"  -> Xml.main (args "")
            "Html" -> Html.main (args "")
            "Js"   -> Js.main (args "")
            "Pnm"  -> Pnm.main (args "")
            "Svg"  -> Svg.main (args "")
            "TTF"  -> TTF.main (args "")
            "CSS"  -> Css.main (args "")
            --"MBox"   -> MBox.main (args "")
            "BS"   -> ByteString.main (args "")
#endif
            _      -> print "Unsupported Type"

-- | Just checks that the command and the action are executables in the current
-- system
safetyChecks :: MainArgs -> IO ()
safetyChecks args = do
    return ()
    --cmdex <- findExecutable cmd
    --unless (isJust cmdex) (die $ "The command \"" ++ cmd ++ "\" is not present.")
    --let act = findAct args
    --actx <- findExecutable act
    --unless (isJust actx) (die $ "The action \"" ++ act ++ "\" cannot be done.")
        
main = do
    interface <- cli
    runApp interface dispatch
