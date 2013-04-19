module Language.Haskell.Exts.Annotated.CPP
  ( parseFileWithComments
  , parseFileContentsWithComments
  , defaultCpphsOptions
  , CpphsOptions(..)
  , BoolOptions(..)
  ) where

import qualified Language.Preprocessor.Cpphs as Orig
import Language.Preprocessor.Cpphs hiding (defaultCpphsOptions)
import Language.Preprocessor.Unlit
import Language.Haskell.Exts (ParseMode(..))
import Language.Haskell.Exts.Annotated
  hiding (parseFileWithComments, parseFileContentsWithComments)
import Control.Applicative
import Data.List

parseFileWithComments ::  CpphsOptions -> ParseMode -> FilePath -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
parseFileWithComments cppopts p fp = readFile fp >>= parseFileContentsWithComments cppopts p { parseFilename = fp }

parseFileContentsWithComments :: CpphsOptions -> ParseMode -> String -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
parseFileContentsWithComments cppopts p@(ParseMode fn exts ign _ _) rawStr =
        let md = delit fn rawStr
            allExts = impliesExts $ case (ign, readExtensions md) of
                                     (False,Just es) -> exts ++ es
                                     _               -> exts
            p' = p { extensions = allExts }
         in parseModuleWithComments p' <$> cpp cppopts p' md

cpp cppopts p str
  | CPP `elem` extensions p
  = runCpphs cppopts (parseFilename p) str
  | otherwise = return str

delit :: String -> String -> String
delit fn = if ".lhs" `isSuffixOf` fn then unlit fn else id

defaultCpphsOptions =
  Orig.defaultCpphsOptions
  { boolopts = (boolopts Orig.defaultCpphsOptions)
      { locations = True
      , stripC89 = True
      , stripEol = False
      , hashline = False
      }
  }
