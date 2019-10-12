module Parser
  ( SvnStatusLine (..)
  , line
  , changeListSep
  , aFile
  , emptySvnLine
  , modificationFlag
  , mParseSvnStatusOutput
  ) where

import Data.Proxy
import Text.ParserCombinators.Parsec as P
import Text.Parsec.Char

import Types

data SvnStatusLine = File SvnFile | ChangelistSeparator String | EmptyLine
    deriving (Show, Eq)

svnStatusOutput :: GenParser Char st [SvnStatusLine]
svnStatusOutput = P.many line

line :: GenParser Char st SvnStatusLine
line = choice [emptySvnLine, changeListSep, aFile]

emptySvnLine :: GenParser Char st SvnStatusLine
emptySvnLine = endOfLine >> return EmptyLine

changeListSep :: GenParser Char st SvnStatusLine
changeListSep = do
  name <- string "--- Changelist '" >> manyTill anyChar (try (string "':" >> endOfLine))
  return (ChangelistSeparator name)

aFile :: GenParser Char st SvnStatusLine
aFile = fmap File $ pure SvnFile
  <*> modificationFlag
  <*> modificationFlag
  <*> modificationFlag
  <*> modificationFlag
  <*> modificationFlag
  <*> modificationFlag
  <*> modificationFlag
  <*> (char ' ' *> manyTill anyChar endOfLine)


mParseSvnStatusOutput :: String -> Either ParseError [SvnStatusLine]
mParseSvnStatusOutput = parse svnStatusOutput "(stdin)"

modificationFlag :: forall st f. SvnFlag f => GenParser Char st f
modificationFlag = do
  flag <- oneOf (possibleValues (Proxy :: Proxy f))
  return $ parseFlag flag

