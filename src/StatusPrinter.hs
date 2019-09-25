module StatusPrinter
  ( showChanges
  ) where


import qualified Data.Map as M
import System.Console.ANSI
import Text.Printf

import Parser
import Types


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


wrapped :: Semigroup a => a -> a -> a -> a
wrapped headList tailList content = headList <> content <> tailList


withStyle :: SGR -> StatusLine -> StatusLine
withStyle lineStyle = wrapped [Style lineStyle] [Style Reset]


withConsoleIntensity :: ConsoleIntensity -> StatusLine -> StatusLine
withConsoleIntensity consoleIntensity = withStyle $ SetConsoleIntensity consoleIntensity


stringToLine :: String -> StatusLine
stringToLine text = [Text text]


header :: FileGroup -> StatusLine
header = return . Text . (++ ":") . hdr where
  hdr :: FileGroup -> String
  hdr Modified = "Modified files"
  hdr Added = "Files added under version control"
  hdr NotTracked = "Not tracked files"
  hdr NotRecognized = "Files not recognized by wrapper. Please report to dmitry.a.bogdanov@gmail.com"


dullColor :: Color -> SGR
dullColor = SetColor Foreground Dull


style :: FileGroup -> SGR
style Modified = dullColor Green
style Added = dullColor Green
style NotTracked = dullColor Red
style NotRecognized = dullColor Blue


showClPart :: FileGroup -> [SvnFile] -> [StatusLine]
showClPart _ [] = []
showClPart grp files = [header grp] ++ map (withStyle (style grp) . tabbed 2 . return . Text  . path) files


showChanges :: Bool -> ChangesModel -> String
showChanges colored m = toPrintableString colored $ M.foldMapWithKey showChangeList m
  where
    showChangeList :: String -> ChangeList -> [StatusLine]
    showChangeList name (ChangeList cl) = let
        headerLines = case name of
          "" -> [ withConsoleIntensity BoldIntensity $ stringToLine "Files to related to any changeslist:"
                ]
          n -> [ withConsoleIntensity BoldIntensity $ stringToLine $ printf "Changelist '%s':" n
               , stringToLine $ printf "  (use \"svn changelist '%s' <file>...\" to add files to this changelist)" n
               , stringToLine $ printf "  (use \"svn changelist --remove <file>...\" to remove files from changelist)"
               , stringToLine $ printf "  (use \"svn commit --cl '%s' -m <message> to commit this changelist)" n
               , stringToLine $ printf ""
               ]
        content = M.foldMapWithKey showClPart cl
      in case content of
        [] -> []
        someLines -> headerLines ++ (map (tabbed 2 ) someLines) ++ [[Text ""]]
