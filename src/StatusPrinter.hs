module StatusPrinter
  ( showChanges
  ) where


import qualified Data.Map as M
import System.Console.ANSI
import Text.Printf

import Parser
import Types


type FilesWithHeader = (String, Color, [SvnFile])

class Printable a where
  toPrintableString :: Bool -> a -> String

data TextElem = Text String
  | Style SGR

instance Printable TextElem where
  toPrintableString _ (Text string) = string
  toPrintableString True (Style sgr) = setSGRCode [sgr]
  toPrintableString False _ = ""


type StatusLine = [TextElem]

instance Printable [TextElem] where
  toPrintableString colored textElems = foldMap (toPrintableString colored) textElems

instance Printable [[TextElem]] where
  toPrintableString colored lns = unlines $ map (toPrintableString colored) lns

tab :: Int -> String
tab = flip replicate ' '

tabbed :: Int -> StatusLine -> StatusLine
tabbed n line = [Text $ tab n] ++ line

wrapped :: [a] -> [a] -> [a] -> [a]
wrapped header footer text = header ++ text ++ footer

withStyle :: SGR -> StatusLine -> StatusLine
withStyle _ [] = []
withStyle style line = wrapped [Style style] [Style Reset] line

withColor :: ColorIntensity -> Color -> StatusLine -> StatusLine
withColor colorIntensity color = withStyle (SetColor Foreground colorIntensity color)

withConsoleIntensity :: ConsoleIntensity -> StatusLine -> StatusLine
withConsoleIntensity consoleIntensity = withStyle $ SetConsoleIntensity consoleIntensity

stringToLine :: String -> StatusLine
stringToLine text = [Text text]

getClContentLines :: ChangeList -> [StatusLine]
getClContentLines cl = foldMap showFilesWithHeader
  [ ("Not tracked files:", Red, notTracked cl)
  , ("Modified files:", Green,  modified cl)
  , ("Files added under version control:", Green, added cl)
  , ("Files not recognized by wrapper :(", Blue, notRecognized cl)
  ]


showFilesWithHeader :: FilesWithHeader -> [StatusLine]
showFilesWithHeader (_, _, []) = []
showFilesWithHeader (header, color, files) = [[Text header]] ++ map (withColor Dull color . tabbed 2 . return . Text  . path) files

showChanges :: Bool -> ChangesModel -> String
showChanges colored m = toPrintableString colored $ M.foldMapWithKey showChangeList m
  where
    showChangeList :: String -> ChangeList -> [StatusLine]
    showChangeList name cl = let
        headerLines = case name of
          "" -> [ withConsoleIntensity BoldIntensity $ stringToLine "Files to related to any changeslist:"
                ]
          n -> [ withConsoleIntensity BoldIntensity $ stringToLine $ printf "Changelist '%s':" n
               , stringToLine $ printf "  (use \"svn changelist '%s' <file>...\" to add files to this changelist)" n
               , stringToLine $ printf "  (use \"svn changelist --remove <file>...\" to remove files from changelist)"
               , stringToLine $ printf "  (use \"svn commit --cl '%s' -m <message> to commit this changelist)" n
               , stringToLine $ printf ""
               ]
        content = getClContentLines cl
      in case content of
        [] -> []
        someLines -> headerLines ++ (map (tabbed 2 ) someLines) ++ [[Text ""]]
