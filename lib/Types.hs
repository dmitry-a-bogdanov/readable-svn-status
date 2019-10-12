module Types
  ( SvnFile (..)
  , SvnFlag
  , parseFlag
  , getFromFile
  , defaultFile
  , possibleValues
  , ModificationStatus (..)
  , PropStatus (..)
  , LockStatus (..)
  , HistoryStatus (..)
  , SwitchStatus (..)
  , LockInfo (..)
  , ConflictStatus(..)
  ) where

import Data.Proxy

class (Eq a) => SvnFlag a where
  parseFlag :: Char -> a
  parseFlag c = case lookup c (charToFlagMapping (Proxy :: Proxy a)) of
    Just f -> f
    Nothing -> error "unknown flag value"

  getFromFile :: SvnFile -> a

  charToFlagMapping :: Proxy a -> [(Char, a)]

  possibleValues :: Proxy a -> [Char]
  possibleValues _ = map fst (charToFlagMapping (Proxy :: Proxy a))

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
  getFromFile = modificationStatus

  charToFlagMapping _ =
    [ (' ', MsNoModification)
    , ('A', MsAdded)
    , ('D', MsDeleted)
    , ('M', MsModified)
    , ('R', MsReplaced)
    , ('C', MsConflict)
    , ('I', MsIgnored)
    , ('?', MsUntracked)
    , ('!', MsMissing)
    , ('~', MsKindChanged)
    ]


data PropStatus = PsNoModification
    | PsModified
    | PsConflict
    deriving (Eq, Show)

instance SvnFlag PropStatus where
  charToFlagMapping _ =
    [ (' ', PsNoModification)
    , ('M', PsModified)
    , ('C', PsConflict)
    ]

  getFromFile = propStatus


data LockStatus = Locked | NotLocked
  deriving (Eq, Show)


instance SvnFlag LockStatus where
  charToFlagMapping _ =
    [ (' ', NotLocked)
    , ('L', Locked)
    ]

  getFromFile = lockStatus

data HistoryStatus = HasHistory | NoHistory
  deriving (Eq, Show)


instance SvnFlag HistoryStatus where
  charToFlagMapping _ =
    [ ('+', HasHistory)
    , (' ', NoHistory)
    ]

  getFromFile = historyStatus


data SwitchStatus = Switched | NotSwitched
  deriving (Eq, Show)


instance SvnFlag SwitchStatus where
  charToFlagMapping _ =
    [ (' ', NotSwitched)
    , ('S', Switched)
    ]

  getFromFile = switchStatus

data LockInfo = LiNotLocked
  | LiLocalLock
  | LiOtherLocked
  | LiStolen
  | LiBroken
  deriving (Eq, Show)


instance SvnFlag LockInfo where
  charToFlagMapping _ =
    [ (' ', LiNotLocked)
    , ('K', LiLocalLock)
    , ('O', LiOtherLocked)
    , ('T', LiStolen)
    , ('B', LiBroken)
    ]
  getFromFile = lockInfo


data ConflictStatus = NoConflict | Conflict
  deriving (Eq, Show)


instance SvnFlag ConflictStatus where
  charToFlagMapping _ =
    [ (' ', NoConflict)
    , ('C', Conflict)
    ]

  getFromFile = conflictStatus


data SvnFile = SvnFile
    { modificationStatus :: ModificationStatus
    , propStatus :: PropStatus
    , lockStatus :: LockStatus
    , historyStatus :: HistoryStatus
    , switchStatus :: SwitchStatus
    , lockInfo :: LockInfo
    , conflictStatus :: ConflictStatus
    , path :: FilePath
    }
    deriving (Show, Eq)

defaultFile :: FilePath -> SvnFile
defaultFile = SvnFile MsNoModification PsNoModification NotLocked NoHistory NotSwitched LiNotLocked NoConflict
