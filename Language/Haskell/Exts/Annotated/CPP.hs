module Language.Haskell.Exts.Annotated.CPP
  ( parseFileWithComments
  , parseFileContentsWithComments
  ) where

import Language.Preprocessor.Cpphs
import Language.Preprocessor.Unlit
import Language.Haskell.Exts (ParseMode(..))
import Language.Haskell.Exts.Annotated
  hiding (parseFileWithComments, parseFileContentsWithComments)
import Control.Applicative
import Data.List

parseFileWithComments ::  CpphsOptions -> ParseMode -> FilePath -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
parseFileWithComments cppopts p fp = readFile fp >>= parseFileContentsWithComments cppopts p

parseFileContentsWithComments :: CpphsOptions -> ParseMode -> String -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
parseFileContentsWithComments cppopts p@(ParseMode fn exts ign _ _) rawStr =
        let md = delit fn rawStr
            allExts = impliesExts $ case (ign, readExtensions md) of
                                     (False,Just es) -> exts ++ es
                                     _               -> exts
         in parseModuleWithComments (p { extensions = allExts }) <$> runCpphs cppopts fn md

cpp cppopts str = runCpphs cppopts str

delit :: String -> String -> String
delit fn = if ".lhs" `isSuffixOf` fn then unlit fn else id
