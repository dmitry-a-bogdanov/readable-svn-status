module Parser where

import Data.Function
import Data.Functor
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import Data.List.Split
import Prelude
import StatusPrinter


class (Eq a) => SvnFlag a where
    parseFlag :: Char -> a

    getFromFile :: SvnFile -> a

cutFlag :: SvnFlag a => String -> (a, String)
cutFlag (flagChar:rest) = (parseFlag flagChar, rest)
cutFlag _ = undefined


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


data SvnFile = SvnFile
    { getModificationStatus :: ModificationStatus
    , getPropStatus :: PropStatus
    , getPath :: FilePath
    }
    deriving (Show, Eq)

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
    & skipChar 6
    & parsePath


isModified :: SvnFile -> Bool
isModified = (MsModified ==) . getModificationStatus

isUntracked :: SvnFile -> Bool
isUntracked = (MsUntracked ==) . getModificationStatus

data ChangeList = ChangeList
    { modified :: [SvnFile]
    , added :: [SvnFile]
    , notTracked :: [SvnFile] -- meaningful only for anonymous changelist. Just to simplify handling.
    , notRecognized :: [SvnFile] -- it's hard to support all svn flags from scratch. here will be files not included
                              -- to well-known lists.
    }
    deriving (Eq, Show)

type FilesWithHeader = (String, [SvnFile])

showFiles :: FilesWithHeader -> Maybe String
showFiles (_, []) = Nothing
showFiles (header, files) = Just $ unlines (header:(map getPath files))

showChangelistContent :: ChangeList -> String
showChangelistContent cl = unlines $ catMaybes $ map showFiles
    [ ("Not tracked files:", notTracked cl)
    , ("Modified files:", modified cl)
    , ("Files added under version control:", added cl)
    , ("Files not recognized by wrapper :(", notRecognized cl)
    ]


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

toFiles :: String -> [SvnFile]
toFiles = lines <&> (fmap parseSvnFile)

data SvnStatusLine = File SvnFile | ChangelistSeparator String | EmptyLine
    deriving (Show, Eq)

isFile :: SvnStatusLine -> Bool
isFile (File _) = True
isFile _ = False

getFile :: SvnStatusLine -> SvnFile
getFile (File f) = f
getFile _ = undefined

isEmptyLine :: SvnStatusLine -> Bool
isEmptyLine EmptyLine = True
isEmptyLine _ = False

isCl :: SvnStatusLine -> Bool
isCl (ChangelistSeparator _) = True
isCl _ = False

getClName :: SvnStatusLine -> String
getClName (ChangelistSeparator name) = name
getClName _ = undefined


parseSvnOutput :: String -> [SvnStatusLine]
parseSvnOutput out = fmap read $ lines out

filterByFlag :: SvnFlag a => a -> [SvnFile] -> [SvnFile]
filterByFlag flag files = filter (\x -> flag == getFromFile x) files

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
                | "--- Changelist " `L.isPrefixOf` string = ChangelistSeparator $ extractChangelistName string
                | string == "" = EmptyLine
                | otherwise = File $ parseSvnFile string
            extractChangelistName string = take (length withoutStartingQuote - 2) withoutStartingQuote
                    where
                        withoutPrefix = dropWhile (/= '\'') string
                        withoutStartingQuote = drop 1 withoutPrefix

type ChangesModel = M.Map String ChangeList


readModel :: String -> ChangesModel
readModel str = build $ parseSvnOutput str

build :: [SvnStatusLine] -> ChangesModel
build svnLines = let
    nonEmptyLines = filter (not . isEmptyLine) svnLines
    splat = split (keepDelimsL $ whenElt isCl) nonEmptyLines
  in
    buildModel splat M.empty
  where
    buildModel :: [[SvnStatusLine]] -> ChangesModel -> ChangesModel
    buildModel [] m = m
    buildModel ([]:ss) m = buildModel ss m
    buildModel (s:ss) m = let
        changelistName = if isCl $ head s then getClName $ head s else ""
      in
        M.insert changelistName (fromList $ map getFile $ filter isFile s) $ buildModel ss m

type Lines = [String]

toString :: ChangesModel -> String
toString m = unlines $ M.foldlWithKey showNonEmpty [] m
    where
        showNonEmpty :: [String] -> String -> ChangeList -> [String]
        showNonEmpty output changelistName cl = let
            header = createHeader changelistName
          in
            output ++ [header, showChangelistContent cl]
          where
            createHeader :: String -> String
            createHeader "" = withStyle BoldBlack "Files to related to any changeslist:"
            createHeader x = unlines
              [ withStyle BoldBlack ("Changelist '" ++ x ++ "':")
              , "  (use \"svn changelist '" ++ x ++ "' <file>...\" to add files to this changelist)"
              , "  (use \"svn changelist --remove <file>...\" to remove files from changelist)"
              , "  (use \"svn commit --cl '" ++ x ++ "' -m <message> to commit this changelist)"
              ]