module Types
  ( SvnFile (..)
  , SvnFlag
  , parseFlag
  , getFromFile
  , defaultFile
  , ModificationStatus (..)
  , PropStatus (..)
  , LockStatus (..)
  , HistoryStatus (..)
  , SwitchStatus (..)
  , LockInfo (..)
  , ConflictStatus(..)
  ) where

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
