module ChangesModel
  ( ChangesModel
  , ChangeList (..)
  , FileGroup (..)
  , parseModel
  ) where

import Control.Applicative
import Data.Bifunctor
import Data.List
import qualified Data.Map as M
import Text.Parsec (ParseError)

import Parser
import Types


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
    fillUnrecognized fs (ChangeList cl) = ChangeList $ M.insert NotRecognized (uniqFiles \\ recognizedFiles) cl
      where
        uniqFiles = nub fs
        recognizedFiles = nub $ M.foldl (++) [] cl



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

modifyCurrentChangeList :: (Maybe [SvnFile] -> Maybe [SvnFile]) -> PState -> PState
modifyCurrentChangeList f state =
  let name = currentChangeListName state
  in state {changeLists = M.alter f name $ changeLists state}

addFileToCurrentChangeList :: SvnFile -> PState -> PState
addFileToCurrentChangeList file = modifyCurrentChangeList $ fmap (++ [file]) . (<|> Just [])

handleOneLine :: PState -> SvnStatusLine -> PState
handleOneLine currentState svnStatusLine =
  let
    stateAction = case svnStatusLine of
      EmptyLine -> id
      ChangelistSeparator changeListName -> setCurrentChangeList changeListName
      File file -> addFileToCurrentChangeList file
  in
    stateAction currentState

buildModel :: [SvnStatusLine] -> ChangesModel
buildModel svnStatusLines = M.map fromList $ changeLists $ foldl handleOneLine emptyState svnStatusLines

parseModel :: String -> Either ParseError ChangesModel
parseModel input = buildModel <$> parseSvnOutput input
