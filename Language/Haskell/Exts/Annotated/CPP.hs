module Language.Haskell.Exts.Annotated.CPP
  ( parseFileWithCommentsAndCPP
  , parseFileContentsWithCommentsAndCPP
  , defaultCpphsOptions
  , CpphsOptions(..)
  , BoolOptions(..)
  ) where

import qualified Debug.Trace as Debug

import qualified Language.Preprocessor.Cpphs as Orig
import Language.Preprocessor.Cpphs hiding (defaultCpphsOptions)
import Language.Preprocessor.Unlit
import Language.Haskell.Exts (ParseMode(..))
import Language.Haskell.Exts.Annotated
import Control.Applicative
import Data.List

parseFileWithCommentsAndCPP ::  CpphsOptions -> ParseMode -> FilePath
                      -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
parseFileWithCommentsAndCPP cppopts parseMode0 file = do
    content <- readFile file
    parseFileContentsWithCommentsAndCPP cppopts parseMode content
  where
    parseMode = parseMode0 { parseFilename = file }

parseFileContentsWithCommentsAndCPP
    :: CpphsOptions -> ParseMode -> String
    -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
parseFileContentsWithCommentsAndCPP cppopts p@(ParseMode fn exts ign _ _) rawStr = do
    let md = delit fn rawStr
        allExts = impliesExts $ case (ign, readExtensions md) of
                                 (False,Just es) -> exts ++ es
                                 _               -> exts
        p' = p { extensions = allExts
               , ignoreLanguagePragmas = False
               }
    print $ ignoreLanguagePragmas p
    processedSrc <- cpp cppopts p' md
    putStrLn processedSrc
    return $ parseFileContentsWithComments p' processedSrc

cpp cppopts p str
  | Debug.trace (show $ extensions p) False = undefined
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
