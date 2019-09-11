module Parser where

import Data.Function
import Data.Functor
import Data.List

class SvnFlag a where
    parseFlag :: Char -> a

cutFlag :: SvnFlag a => String -> (a, String)
cutFlag (flagChar:rest) = (parseFlag flagChar, rest)


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


data PropStatus = PsNoModification
    | PsModified
    | PsConflict
    deriving (Eq, Show)

instance SvnFlag PropStatus where
    parseFlag ' ' = PsNoModification
    parseFlag 'M' = PsModified
    parseFlag 'C' = PsConflict
    parseFlag _ = undefined


data DummyFlag = DummyFlag
    deriving (Eq, Show)

instance SvnFlag DummyFlag where
    parseFlag _ = DummyFlag


data SvnFile = SvnFile
    { getModificationStatus :: ModificationStatus
    , getPropStatus :: PropStatus
    , getPath :: FilePath
    }
    deriving (Show)

data ParsingState c = ParsingState String c

parseOneFlag (ParsingState (c:chrs) ctor) = ParsingState chrs (ctor $ parseFlag c)

parsePath (ParsingState str ctor) = ctor str

skipChar n (ParsingState str ctor) = ParsingState (drop n str) ctor


parseSvnStatusLine :: String -> SvnFile
parseSvnStatusLine str = (parseOneFlag (ParsingState str SvnFile))
    & parseOneFlag
    & skipChar 6
    & parsePath


isModified :: SvnFile -> Bool
isModified = (MsModified ==) . getModificationStatus

isUntracked :: SvnFile -> Bool
isUntracked = (MsUntracked ==) . getModificationStatus

tab :: Int -> String
tab = flip replicate ' '

class ChangesModel a where
    build :: [SvnFile] -> a
    toString :: a -> String

data NoChangelistModel = NoChangelistModel [SvnFile]

data TextStyle = Escape | Red | Green | Blue | Reset | BoldBlack deriving (Show)

getString :: TextStyle -> String
getString Escape = "\x1b["
getString Red = "0;31m"
getString Green = "0;32m"
getString Blue = "0;34m"
getString BoldBlack = "1;30m"
getString Reset = "0m"


withStyle :: TextStyle -> String -> String
withStyle style text = (getString Escape) <> (getString style) <> text <> (getString Escape) <> (getString Reset)





instance ChangesModel NoChangelistModel where
    build files = NoChangelistModel files

    toString (NoChangelistModel files) = (showFiles (isModified) (withStyle Blue) "Modified files:" files) ++
        "\n" ++
        (showFiles (isUntracked) (withStyle Red) (withStyle BoldBlack "Untracked files:") files)
        where
            showFiles :: (SvnFile -> Bool) -> (String -> String) -> String -> [SvnFile] -> String
            showFiles predicate styler header files = let
                    filesToShow = filter predicate files
                    fileNames = map getPath filesToShow
                    coloredFileNames = map styler fileNames
                    fileRows = map (\x -> (tab 4) ++ x ++ "\n") coloredFileNames
                in
                    if null fileRows then "" else header <> "\n" <> concat fileRows


toFiles :: String -> [SvnFile]
toFiles = lines <&> (fmap parseSvnStatusLine)

readModel :: ChangesModel a => String -> a
readModel str = build $ toFiles str


