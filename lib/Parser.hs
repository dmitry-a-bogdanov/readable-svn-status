module Parser
  ( ChangesModel
  , parseFileLists
  , parseLine
  , SvnStatusLine (..)
  , ChangeList (..)
  , defaultFile
  , FileGroup (..)
  ) where

import Control.Applicative
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Bifunctor
import qualified Data.List as L
import qualified Data.Map as M
import Prelude
import Types


data ParsingState c = ParsingState String c

parseOneFlag :: SvnFlag t => ParsingState (t -> c) -> ParsingState c
parseOneFlag (ParsingState (c:chrs) ctor) = ParsingState chrs (ctor $ parseFlag c)
parseOneFlag _ = error "Malformed input"

parsePath :: ParsingState (String -> t) -> t
parsePath (ParsingState str ctor) = ctor str

skipChar :: Int -> ParsingState c -> ParsingState c
skipChar n (ParsingState str ctor) = ParsingState (drop n str) ctor


parseSvnFile :: String -> SvnFile
parseSvnFile str = (ParsingState str SvnFile)
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


matchesAll :: [a -> Bool] -> (a -> Bool)
matchesAll preds = \x -> and $ map ($ x) preds


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
        uniqFiles = L.nub $ fs
        recognizedFiles = L.nub $ M.foldl (++) [] cl


data SvnStatusLine = File SvnFile | ChangelistSeparator String | EmptyLine
    deriving (Show, Eq)


hasFlag :: SvnFlag a => a -> SvnFile -> Bool
hasFlag flag = (flag ==) . getFromFile


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

emptyState :: PState
emptyState = PState "" M.empty

setCurrentChangeList :: String -> PState -> PState
setCurrentChangeList name pstate = pstate { currentChangeListName = name }

addFileToCurrentChangeList :: SvnFile -> PState -> PState
addFileToCurrentChangeList f pstate =
  let
    clName = currentChangeListName pstate
  in pstate {
    changeLists = M.alter (fmap (++ [f]) . (<|> (Just []))) clName $ changeLists pstate
  }

parseFileLists :: String -> ChangesModel
parseFileLists string = M.map fromList $ changeLists $ (foldl parseOneLine emptyState) $ parseLine <$> lines string
  where
    parseOneLine :: PState -> SvnStatusLine -> PState
    parseOneLine currentState line = let
        stateAction = case line of
          EmptyLine -> id
          ChangelistSeparator changeListName -> setCurrentChangeList changeListName
          File file -> addFileToCurrentChangeList file
      in
        stateAction currentState

