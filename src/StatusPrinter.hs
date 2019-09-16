module StatusPrinter
  ( toString
  ) where

import Data.Maybe
import qualified Data.Map as M
import Parser

tab :: Int -> String
tab = flip replicate ' '

tabbed :: Int -> String -> String
tabbed n text = (tab n) ++ text

data TextStyle = Escape | Red | Green | Blue | Reset | BoldBlack deriving (Show)

getString :: TextStyle -> String
getString Escape = "\x1b["
getString Red = "0;31m"
getString Green = "0;32m"
getString Blue = "0;34m"
getString BoldBlack = "1;30m"
getString Reset = "0m"


type FilesWithHeader = (String, [SvnFile])


showFiles :: FilesWithHeader -> Maybe String
showFiles (_, []) = Nothing
showFiles (header, files) = Just $ unlines (header:(map getPath files))


withStyle :: TextStyle -> String -> String
withStyle style text = (getString Escape) <> (getString style) <> text <> (getString Escape) <> (getString Reset)

emptyChangeList :: ChangeList
emptyChangeList = ChangeList [] [] [] []


showChangelistContent :: ChangeList -> String
showChangelistContent cl = unlines $ catMaybes $ map showFiles $ --filter (\(header, files) -> not $ null files)
    [ ("Not tracked files:", notTracked cl)
    , ("Modified files:", modified cl)
    , ("Files added under version control:", added cl)
    , ("Files not recognized by wrapper :(", notRecognized cl)
    ]


toString :: ChangesModel -> String
toString m = unlines $ M.foldlWithKey showNonEmpty [] m
    where
        showNonEmpty :: [String] -> String -> ChangeList -> [String]
        showNonEmpty output changelistName cl = let
            header = createHeader changelistName
          in
            if cl == emptyChangeList then output else output ++ [header, showChangelistContent cl]
          where
            createHeader :: String -> String
            createHeader "" = withStyle BoldBlack "Files to related to any changeslist:"
            createHeader x = unlines
              [ withStyle BoldBlack ("Changelist '" ++ x ++ "':")
              , "  (use \"svn changelist '" ++ x ++ "' <file>...\" to add files to this changelist)"
              , "  (use \"svn changelist --remove <file>...\" to remove files from changelist)"
              , "  (use \"svn commit --cl '" ++ x ++ "' -m <message> to commit this changelist)"
              ]
