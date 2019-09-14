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


parseSvnFile :: String -> SvnFile
parseSvnFile str = (parseOneFlag (ParsingState str SvnFile))
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

data NoChangelistModel = NoChangelistModel
    { modifiedFiles :: [SvnFile]
    , untrackedFiles :: [SvnFile]
    }

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
    build files = NoChangelistModel
        (filter isModified files)
        (filter isUntracked files)

    toString model = (showFiles (withStyle Blue) "Modified files:" (modifiedFiles model)) ++
        "\n" ++
        (showFiles (withStyle Red) (withStyle BoldBlack "Untracked files:") (untrackedFiles model))
        where
            showFiles :: (String -> String) -> String -> [SvnFile] -> String
            showFiles styler header files = let
                    fileNames = map getPath files
                    coloredFileNames = map styler fileNames
                    fileRows = map (\x -> (tab 4) ++ x ++ "\n") coloredFileNames
                in
                    if null fileRows then "" else header <> "\n" <> concat fileRows

data ChangeList = ChangeList
    { clModifiedFiles :: [SvnFile]
    , clAddedFiles :: [SvnFile]
    }

toFiles :: String -> [SvnFile]
toFiles = lines <&> (fmap parseSvnFile)

data SvnStatusLine = File SvnFile | ChangelistSeparator String | EmptyLine
    deriving (Show)

instance Read SvnStatusLine where
    readsPrec _ str = let
            isLineBreak = ('\n' ==)
            brokenStr = break isLineBreak str
            line = fst brokenStr
            rest = dropWhile isLineBreak $ snd brokenStr
        in
            [((parse line), rest)]
        where
            parse string
                | "--- Changelist " `isPrefixOf` string = ChangelistSeparator $ extractChangelistName string
                | string == "" = EmptyLine
                | otherwise = File $ parseSvnFile string
            extractChangelistName string = take (length withoutStartingQuote - 2) withoutStartingQuote
                    where
                        withoutPrefix = dropWhile (/= '\'') string
                        withoutStartingQuote = drop 1 withoutPrefix

readModel :: ChangesModel a => String -> a
readModel str = build $ toFiles str


