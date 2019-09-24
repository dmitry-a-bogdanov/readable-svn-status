module StatusPrinter
  ( showChanges
  ) where


import qualified Data.Map as M
import Text.Printf

import Parser
import Types


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


type FilesWithHeader = (String, TextStyle, [SvnFile])


withStyle :: TextStyle -> String -> String
withStyle style text = (getString Escape) <> (getString style) <> text <> (getString Escape) <> (getString Reset)

withStyleCond :: Bool -> TextStyle -> String -> String
withStyleCond colored = if colored
    then withStyle
    else (\_ -> id)

getClContentLines :: Bool -> ChangeList -> [String]
getClContentLines colored cl = mconcat $ map (showFilesWithHeader colored) $
    [ ("Not tracked files:", Red, notTracked cl)
    , ("Modified files:", Green,  modified cl)
    , ("Files added under version control:", Green, added cl)
    , ("Files not recognized by wrapper :(", Blue, notRecognized cl)
    ]


showFilesWithHeader :: Bool -> FilesWithHeader -> [String]
showFilesWithHeader _ (_, _, []) = []
showFilesWithHeader colored (header, style, files) = [header] ++ map (tabbed 2 . (withStyleCond colored style) . path) files

showChanges :: Bool -> ChangesModel -> String
showChanges colored m = unlines $ M.foldMapWithKey showChangeList m
  where
    showChangeList :: String -> ChangeList -> [String]
    showChangeList name cl = let
          headerLines = case name of
            "" -> [ withStyleCond colored BoldBlack "Files to related to any changeslist:"
                  ]
            n -> [ withStyleCond colored BoldBlack $ printf "Changelist '%s':" n
                 , printf "  (use \"svn changelist '%s' <file>...\" to add files to this changelist)" n
                 , printf "  (use \"svn changelist --remove <file>...\" to remove files from changelist)"
                 , printf "  (use \"svn commit --cl '%s' -m <message> to commit this changelist)" n
                 , printf ""
                 ]
          content = getClContentLines colored cl
        in case content of
          [] -> []
          someLines -> headerLines ++ map (tabbed 2) someLines ++ [""]
