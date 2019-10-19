module ChangesModel
  ( ChangesModel
  , ChangeList (..)
  , FileGroup (..)
  , parseModel
  ) where

import Control.Applicative
import Data.Bifunctor
import Data.Foldable
import Data.List
import Control.Monad.Trans.State
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

type RawChangeLists = M.Map String [SvnFile]

addLine :: RawChangeLists -> SvnStatusLine -> State String RawChangeLists
addLine cls EmptyLine = return cls
addLine cls (ChangelistSeparator clName) = do
  put clName
  return cls
addLine cls (File f) = do
  clName <- get
  return $ M.alter (fmap (++ [f]) . (<|> Just [])) clName cls

type ChangesModel = M.Map String ChangeList

buildModel :: [SvnStatusLine] -> ChangesModel
buildModel svnStatusLines = M.map fromList $ evalState (foldlM addLine M.empty svnStatusLines) ""

parseModel :: String -> Either ParseError ChangesModel
parseModel input = buildModel <$> parseSvnOutput input
