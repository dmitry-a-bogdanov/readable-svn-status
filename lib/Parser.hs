module Parser
  ( ChangesModel
  , SvnStatusLine (..)
  , ChangeList (..)
  , defaultFile
  , FileGroup (..)
  , line
  , changeListSep
  , aFile
  , emptySvnLine
  , parseModel
  ) where

import Control.Applicative as A
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Bifunctor
import Data.Either
import qualified Data.List as L
import qualified Data.Map as M
import Prelude
import Types

import Text.ParserCombinators.Parsec as P
import Text.Parsec.Char

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
aFile = do
  flags <- count 7 anyChar <* char ' '
  fpath <- manyTill anyChar (try endOfLine)
  return $ File . parseSvnFile $ flags ++ " " ++ fpath


mParseSvnStatusOutput :: String -> Either ParseError [SvnStatusLine]
mParseSvnStatusOutput = parse svnStatusOutput "(stdin)"


data ParsingState c = ParsingState String c

parseOneFlag :: SvnFlag t => ParsingState (t -> c) -> ParsingState c
parseOneFlag (ParsingState (c:chrs) ctor) = ParsingState chrs (ctor $ parseFlag c)
parseOneFlag _ = error "Malformed input"

parsePath :: ParsingState (String -> t) -> t
parsePath (ParsingState str ctor) = ctor str

skipChar :: Int -> ParsingState c -> ParsingState c
skipChar n (ParsingState str ctor) = ParsingState (drop n str) ctor


parseSvnFile :: String -> SvnFile
parseSvnFile str = ParsingState str SvnFile
    & parseOneFlag
    & parseOneFlag
    & parseOneFlag
    & parseOneFlag
    & parseOneFlag
    & parseOneFlag
    & parseOneFlag
    & skipChar 1
    & parsePath


data FileGroup
  = Modified
  | ModifiedProperties
  | Added
  | Deleted
  | NotRecognized
  | Conflicted
  | NotTracked
  | NotTouched
  deriving (Bounded, Enum, Eq, Ord, Show)


newtype ChangeList = ChangeList (M.Map FileGroup [SvnFile])
    deriving (Eq, Show)


matchesAll :: [a -> Bool] -> a -> Bool
matchesAll preds x = all ($ x) preds


fromList :: [SvnFile] -> ChangeList
fromList files =
    fillUnrecognized files $ fillRecognized files
  where
    fillRecognized :: [SvnFile] -> ChangeList
    fillRecognized fs = ChangeList $ M.fromList $ map (second $ flip filter fs)
      [ (Modified, hasFlag MsModified)
      , (Added, hasFlag MsAdded)
      , (NotTracked, hasFlag MsUntracked)
      , (NotTouched, matchesAll
          [ hasFlag MsNoModification
          , hasFlag PsNoModification
          , hasFlag NotLocked
          , hasFlag NoHistory
          , hasFlag NotSwitched
          , hasFlag LiNotLocked
          , hasFlag NoConflict
          ])
      , (ModifiedProperties, hasFlag PsModified)
      , (Deleted, hasFlag MsDeleted)
      , (Conflicted, hasFlag MsConflict)
      ]

    fillUnrecognized :: [SvnFile] -> ChangeList -> ChangeList
    fillUnrecognized fs (ChangeList cl) = ChangeList $ M.insert NotRecognized (uniqFiles L.\\ recognizedFiles) cl
      where
        uniqFiles = L.nub fs
        recognizedFiles = L.nub $ M.foldl (++) [] cl


data SvnStatusLine = File SvnFile | ChangelistSeparator String | EmptyLine
    deriving (Show, Eq)


hasFlag :: SvnFlag a => a -> SvnFile -> Bool
hasFlag flag = (flag ==) . getFromFile

type ChangesModel = M.Map String ChangeList


data PState = PState
  { currentChangeListName :: String
  , changeLists :: M.Map String [SvnFile]
  }

emptyState :: PState
emptyState = PState "" M.empty

setCurrentChangeList :: String -> PState -> PState
setCurrentChangeList name pstate = pstate { currentChangeListName = name }

addFileToCurrentChangeList :: SvnFile -> PState -> PState
addFileToCurrentChangeList f pstate =
  let
    clName = currentChangeListName pstate
  in pstate {
    changeLists = M.alter (fmap (++ [f]) . (A.<|> (Just []))) clName $ changeLists pstate
  }

buildModel :: [SvnStatusLine] -> ChangesModel
buildModel svnStatusLines = M.map fromList $ changeLists $ foldl parseOneLine emptyState svnStatusLines
  where
    parseOneLine :: PState -> SvnStatusLine -> PState
    parseOneLine currentState svnStatusLine = let
        stateAction = case svnStatusLine of
          EmptyLine -> id
          ChangelistSeparator changeListName -> setCurrentChangeList changeListName
          File file -> addFileToCurrentChangeList file
      in
        stateAction currentState

parseModel :: String -> Either ParseError ChangesModel
parseModel input = buildModel <$> mParseSvnStatusOutput input

