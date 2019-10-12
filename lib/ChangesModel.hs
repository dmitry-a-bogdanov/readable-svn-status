module ChangesModel
  ( ChangesModel
  , ChangeList (..)
  , FileGroup (..)
  , parseModel
  ) where

import Control.Applicative as A
import Data.List as L
import Data.Map as M
import Text.ParserCombinators.Parsec
import Data.Bifunctor

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
    fillRecognized fs = ChangeList $ M.fromList $ L.map (second $ flip L.filter fs)
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
buildModel svnStatusLines = M.map ChangesModel.fromList $ changeLists $ L.foldl parseOneLine emptyState svnStatusLines
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
