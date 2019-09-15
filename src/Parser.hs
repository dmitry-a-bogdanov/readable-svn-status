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
    { clModifiedFiles :: [SvnFile]
    , clAddedFiles :: [SvnFile]
    , notTracked :: [SvnFile] -- meaningful only for anonymous changelist. Just to simplify handling
    }
    deriving (Eq, Show)

type FilesWithHeader = (String, [SvnFile])

showFiles :: FilesWithHeader -> Maybe String
showFiles (_, []) = Nothing
showFiles (header, files) = Just $ unlines (header:(map getPath files))

showChangelistContent :: ChangeList -> String
showChangelistContent cl = unlines $ catMaybes $ map showFiles
    [ ("Not tracked files:", notTracked cl)
    , ("Modified files:", clModifiedFiles cl)
    , ("Files added under version control: ", clAddedFiles cl)
    ]


fromList :: [SvnFile] -> ChangeList
fromList files = ChangeList
    (filterByFlag MsModified files)
    (filterByFlag MsAdded files)
    (filterByFlag MsUntracked files)

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

readModel :: String -> ClOnTopModel
readModel str = build $ parseSvnOutput str

data ClOnTopModel = ClOnTopModel
    { xUntracked :: [SvnFile]
    , xModified :: [SvnFile]
    , changeLists :: M.Map String ChangeList
    }
    deriving (Eq, Show)

addFiles :: [SvnFile] -> ClOnTopModel -> (ClOnTopModel -> [SvnFile]) -> [SvnFile]
addFiles files model getter = (getter model) ++ files


withUntracked :: [SvnFile] -> ClOnTopModel -> ClOnTopModel
withUntracked files m = m { xUntracked = addFiles files m xUntracked }


withModifed :: [SvnFile] -> ClOnTopModel -> ClOnTopModel
withModifed files m = m { xModified = addFiles files m xModified }


build :: [SvnStatusLine] -> ClOnTopModel
build svnLines = let
    nonEmptyLines = filter (not . isEmptyLine) svnLines
    splat = split (keepDelimsL $ whenElt isCl) nonEmptyLines
  in
    buildModel splat $ ClOnTopModel [] [] $ M.empty
  where
    buildModel :: [[SvnStatusLine]] -> ClOnTopModel -> ClOnTopModel
    buildModel [] m = m
    buildModel ([]:ss) m = buildModel ss m
    buildModel (s:ss) m = if isCl $ head s
      then m
      else buildModel ss $ withUntracked untrackedHere
        $ withModifed modifiedHere
        $ m
        where
          files = map getFile $ filter isFile s
          modifiedHere = filterByFlag MsModified files
          untrackedHere = filterByFlag MsUntracked files

toString :: ClOnTopModel -> String
toString m = unlines $ catMaybes
        [ Just $ withStyle BoldBlack "Files not related to any changelist:"
        , showNonEmpty "Not tracked files:" Red $ xUntracked m
        , showNonEmpty "Modified files:" Green $ xModified m
        ]
    where
        showNonEmpty :: String -> TextStyle -> [SvnFile] -> Maybe String
        showNonEmpty header filesStyle files = if null files
            then Nothing
            else Just $ unlines (header:(map (\f -> tabbed 4 $ withStyle filesStyle $ getPath f) files))
