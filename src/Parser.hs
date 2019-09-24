module Parser
  ( ChangesModel
  , parseFileLists
  , parseLine
  , SvnStatusLine (..)
  , ModificationStatus (..)
  , SvnFile (..)
  , PropStatus (..)
  , LockStatus (..)
  , HistoryStatus (..)
  , SwitchStatus (..)
  , LockInfo (..)
  , ConflictStatus(..)
  , ChangeList (..)
  , defaultFile
  ) where

import Data.Function
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import Prelude


class (Eq a) => SvnFlag a where
    parseFlag :: Char -> a

    getFromFile :: SvnFile -> a


data ModificationStatus = MsNoModification
    | MsAdded
    | MsDeleted
    | MsModified
    | MsReplaced
    | MsConflict
    | MsExternal
    | MsIgnored
    | MsUntracked
    | MsMissing
    | MsKindChanged
    deriving (Eq, Show)

instance SvnFlag ModificationStatus where
    parseFlag ' ' = MsNoModification
    parseFlag 'A' = MsAdded
    parseFlag 'D' = MsDeleted
    parseFlag 'M' = MsModified
    parseFlag 'R' = MsReplaced
    parseFlag 'C' = MsConflict
    parseFlag 'X' = MsExternal
    parseFlag 'I' = MsIgnored
    parseFlag '?' = MsUntracked
    parseFlag '!' = MsMissing
    parseFlag '~' = MsKindChanged
    parseFlag _ = undefined

    getFromFile = getModificationStatus


data PropStatus = PsNoModification
    | PsModified
    | PsConflict
    deriving (Eq, Show)

instance SvnFlag PropStatus where
    parseFlag ' ' = PsNoModification
    parseFlag 'M' = PsModified
    parseFlag 'C' = PsConflict
    parseFlag _ = undefined

    getFromFile = getPropStatus


data LockStatus = Locked | NotLocked
  deriving (Eq, Show)


instance SvnFlag LockStatus where
  parseFlag ' ' = NotLocked
  parseFlag 'L' = Locked
  parseFlag _ = error "Unknown locked status"

  getFromFile = locked

data HistoryStatus = HasHistory | NoHistory
  deriving (Eq, Show)


instance SvnFlag HistoryStatus where
  parseFlag '+' = HasHistory
  parseFlag ' ' = NoHistory
  parseFlag _ = error "Unknown history flag"

  getFromFile = history


data SwitchStatus = Switched | NotSwitched
  deriving (Eq, Show)


instance SvnFlag SwitchStatus where
  parseFlag ' ' = NotSwitched
  parseFlag 'S' = Switched
  parseFlag _ = error "Unknown switched status"

  getFromFile = switchStatus

data LockInfo = LiNotLocked
  | LiLocalLock
  | LiOtherLocked
  | LiStolen
  | LiBroken
  deriving (Eq, Show)


instance SvnFlag LockInfo where
  parseFlag ' ' = LiNotLocked
  parseFlag 'K' = LiLocalLock
  parseFlag 'O' = LiOtherLocked
  parseFlag 'T' = LiStolen
  parseFlag 'B' = LiBroken
  parseFlag _ = error "Unknown lock info"

  getFromFile = lockInfo


data ConflictStatus = NoConflict | Conflict
  deriving (Eq, Show)


instance SvnFlag ConflictStatus where
  parseFlag ' ' = NoConflict
  parseFlag 'C' = Conflict
  parseFlag _ = error "Unknown conflict status"

  getFromFile = conflict


data SvnFile = SvnFile
    { getModificationStatus :: ModificationStatus
    , getPropStatus :: PropStatus
    , locked :: LockStatus
    , history :: HistoryStatus
    , switchStatus :: SwitchStatus
    , lockInfo :: LockInfo
    , conflict :: ConflictStatus
    , getPath :: FilePath
    }
    deriving (Show, Eq)

defaultFile :: FilePath -> SvnFile
defaultFile = SvnFile MsNoModification PsNoModification NotLocked NoHistory NotSwitched LiNotLocked NoConflict

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


data ChangeList = ChangeList
    { modified :: [SvnFile]
    , added :: [SvnFile]
    , notTracked :: [SvnFile]  {- meaningful only for anonymous changelist. Just to simplify handling. -}
    , notRecognized :: [SvnFile]  {- it's hard to support all svn flags from scratch. here will be files not included
                                     to well-known lists. -}
    }
    deriving (Eq, Show)


fromList :: [SvnFile] -> ChangeList
fromList files =
    fillUnrecognized files $ fillRecognized files
  where
    fillRecognized :: [SvnFile] -> ChangeList
    fillRecognized fs = ChangeList
      (filterByFlag MsModified fs)
      (filterByFlag MsAdded fs)
      (filterByFlag MsUntracked fs)
      []

    fillUnrecognized :: [SvnFile] -> ChangeList -> ChangeList
    fillUnrecognized fs cl = cl { notRecognized = uniqFiles L.\\ recognizedFiles }
      where
        uniqFiles = L.nub $ fs
        recognizedFiles = L.nub $ concat $ map ($ cl)
            [ notTracked
            , modified
            , added
            ]


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

