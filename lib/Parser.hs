module Parser
  ( SvnStatusLine (..)
  , svnStatusLineParser
  , changeListHeaderParser
  , svnFileParser
  , emptyLineParser
  , svnFileFlagParser
  , parseSvnOutput
  ) where

import Data.Proxy
import Text.ParserCombinators.Parsec
import Text.Parsec.Char

import Types

data SvnStatusLine = File SvnFile | ChangelistSeparator String | EmptyLine
    deriving (Show, Eq)

svnStatusOutputParser :: GenParser Char st [SvnStatusLine]
svnStatusOutputParser = many svnStatusLineParser

svnStatusLineParser :: GenParser Char st SvnStatusLine
svnStatusLineParser = choice [emptyLineParser, changeListHeaderParser, svnFileParser]

emptyLineParser :: GenParser Char st SvnStatusLine
emptyLineParser = endOfLine >> return EmptyLine

changeListHeaderParser :: GenParser Char st SvnStatusLine
changeListHeaderParser = do
  name <- string "--- Changelist '" >> manyTill anyChar (try (string "':" >> endOfLine))
  return (ChangelistSeparator name)

svnFileParser :: GenParser Char st SvnStatusLine
svnFileParser = fmap File $ pure SvnFile
  <*> svnFileFlagParser
  <*> svnFileFlagParser
  <*> svnFileFlagParser
  <*> svnFileFlagParser
  <*> svnFileFlagParser
  <*> svnFileFlagParser
  <*> svnFileFlagParser
  <*> (char ' ' *> manyTill anyChar endOfLine)

svnFileFlagParser :: forall st f. SvnFlag f => GenParser Char st f
svnFileFlagParser = do
  flag <- oneOf (possibleValues (Proxy :: Proxy f))
  return $ parseFlag flag

parseSvnOutput :: String -> Either ParseError [SvnStatusLine]
parseSvnOutput = parse svnStatusOutputParser "(stdin)"
