module StatusPrinter
  ( showChanges
  ) where


import qualified Data.Map as M
import Text.Printf

import Parser


tab :: Int -> String
tab = flip replicate ' '

tabbed :: Int -> String -> String
tabbed n text = (tab n) ++ text

data TextStyle =  Escape | Red |  Green | Blue | Reset | BoldBlack deriving (Show)

getString :: TextStyle -> String
getString Escape = "\x1b["
getString Red = "0;31m"
getString Green = "0;32m"
getString Blue = "0;34m"
getString BoldBlack = "1;30m"
getString Reset = "0m"


type FilesWithHeader = (String, [SvnFile])


withStyle :: TextStyle -> String -> String
withStyle style text = (getString Escape) <> (getString style) <> text <> (getString Escape) <> (getString Reset)


getClContentLines :: ChangeList -> [String]
getClContentLines cl = mconcat $ map showFilesWithHeader $ --filter (\(header, files) -> not $ null files)
    [ ("Not tracked files:", notTracked cl)
    , ("Modified files:", modified cl)
    , ("Files added under version control:", added cl)
    , ("Files not recognized by wrapper :(", notRecognized cl)
    ]


showFilesWithHeader :: FilesWithHeader -> [String]
showFilesWithHeader (_, []) = []
showFilesWithHeader (header, files) = [header] ++ map (tabbed 2 . getPath) files


showChanges :: ChangesModel -> String
showChanges m = unlines $ M.foldMapWithKey showChangeList m
  where
    showChangeList :: String -> ChangeList -> [String]
    showChangeList name cl = let
          headerLines = case name of
            "" -> [ "Files to related to any changeslist:"
                  ]
            n -> [ printf "Changelist '%s':" n
                 , printf "  (use \"svn changelist '%s' <file>...\" to add files to this changelist)" n
                 , printf "  (use \"svn changelist --remove <file>...\" to remove files from changelist)"
                 , printf "  (use \"svn commit --cl '%s' -m <message> to commit this changelist)" n
                 , printf ""
                 ]
          content = getClContentLines cl
        in case content of
          [] -> []
          someLines -> headerLines ++ map (tabbed 2) someLines ++ [""]
