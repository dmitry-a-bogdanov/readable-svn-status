module Parser
  ( ChangesModel
  , parseFileLists
  , parseLine
  , SvnStatusLine (..)
  , ChangeList (..)
  , defaultFile
  , FileGroup (..)
  ) where

import Data.Function
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import Prelude
import Types


data ParsingState c = ParsingState String c

parseOneFlag :: SvnFlag t => ParsingState (t -> c) -> ParsingState c
parseOneFlag (ParsingState (c:chrs) ctor) = ParsingState chrs (ctor $ parseFlag c)
parseOneFlag _ = undefined

parsePath :: ParsingState (String -> t) -> t
parsePath (ParsingState str ctor) = ctor str

skipChar :: Int -> ParsingState c -> ParsingState c
skipChar n (ParsingState str ctor) = ParsingState (drop n str) ctor



parseSvnFile :: String -> SvnFile
parseSvnFile str = (parseOneFlag (ParsingState str SvnFile))
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
  | Added
  | NotTracked
  | NotRecognized
  deriving (Bounded, Enum, Eq, Ord, Show)


newtype ChangeList = ChangeList (M.Map FileGroup [SvnFile])
    deriving (Eq, Show)


fromList :: [SvnFile] -> ChangeList
fromList files =
    fillUnrecognized files $ fillRecognized files
  where
    fillRecognized :: [SvnFile] -> ChangeList
    fillRecognized fs = ChangeList $ M.fromList
      [ (Modified, filterByFlag MsModified fs)
      , (Added, filterByFlag MsAdded fs)
      , (NotTracked, filterByFlag MsUntracked fs)
      ]

    fillUnrecognized :: [SvnFile] -> ChangeList -> ChangeList
    fillUnrecognized fs (ChangeList cl) = ChangeList $ M.insert NotRecognized (uniqFiles L.\\ recognizedFiles) cl
      where
        uniqFiles = L.nub $ fs
        recognizedFiles = L.nub $ M.foldl (++) [] cl


data SvnStatusLine = File SvnFile | ChangelistSeparator String | EmptyLine
    deriving (Show, Eq)


filterByFlag :: SvnFlag a => a -> [SvnFile] -> [SvnFile]
filterByFlag flag files = filter ((flag ==) . getFromFile) files


parseLine :: String -> SvnStatusLine
parseLine string   | "--- Changelist " `L.isPrefixOf` string = ChangelistSeparator $ extractChangelistName string
                   | string == "" = EmptyLine
                   | otherwise = File $ parseSvnFile string
                   where
                     extractChangelistName s = take (length withoutStartingQuote - 2) withoutStartingQuote
                       where
                         withoutPrefix = dropWhile (/= '\'') s
                         withoutStartingQuote = drop 1 withoutPrefix

type ChangesModel = M.Map String ChangeList


data PState = PState
  { currentChangeListName :: String
  , changeLists :: M.Map String [SvnFile]
  }


withFileInCl :: String -> SvnFile -> (M.Map String [SvnFile]) -> M.Map String [SvnFile]
withFileInCl clName file = flip M.alter clName $ Just . maybe [file] (++ [file])


parseFileLists :: String -> ChangesModel
parseFileLists string = M.map fromList $ changeLists $ foldl parseOneLine (PState "" M.empty) $ lines string
  where
    parseOneLine :: PState -> String -> PState
    parseOneLine currentState line = case parseLine line of
      EmptyLine -> currentState
      (ChangelistSeparator changeListName) -> currentState { currentChangeListName = changeListName }
      (File file) -> let
          changeListName = currentChangeListName currentState
        in
          currentState { changeLists = withFileInCl changeListName file $ changeLists currentState }

