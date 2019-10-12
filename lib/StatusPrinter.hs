module StatusPrinter
  ( showChanges
  ) where


import qualified Data.Map as M
import System.Console.ANSI
import Text.Printf

import ChangesModel
import Types


class Printable a where
  toPrintableString :: Bool -> a -> String


data TextElem = Text String | Style SGR


instance Printable TextElem where
  toPrintableString _ (Text string) = string
  toPrintableString True (Style sgr) = setSGRCode [sgr]
  toPrintableString False _ = ""


type StatusLine = [TextElem]


instance Printable [TextElem] where
  toPrintableString colored = foldMap (toPrintableString colored)


instance Printable [[TextElem]] where
  toPrintableString colored lns = unlines $ map (toPrintableString colored) lns


tabbed :: Int -> StatusLine -> StatusLine
tabbed n line = (Text $ replicate n ' '):line


withStyle :: SGR -> StatusLine -> StatusLine
withStyle lineStyle line = [Style lineStyle] ++ line ++ [Style Reset]


stringToLine :: String -> StatusLine
stringToLine text = [Text text]


showFilesGroup :: FileGroup -> [SvnFile] -> [StatusLine]
showFilesGroup _ [] = []
showFilesGroup grp files = let
    (headerText, filesStyle) = fgShowProperties grp
    headerLine = stringToLine . (++ ":") $ headerText
    styledFiles = map (withStyle filesStyle . stringToLine . path) files
  in
    headerLine:map (tabbed 2) styledFiles
  where
    fgShowProperties :: FileGroup -> (String, SGR)
    fgShowProperties fg = case fg of
      Modified           -> ("Modified files", dullColor Green)
      Added              -> ("Files added under version control", dullColor Green)
      NotTracked         -> ("Not tracked files", dullColor Red)
      NotRecognized      -> ("Files not recognized by wrapper. Please report to dmitry.a.bogdanov@gmail.com",
                             dullColor Blue)
      NotTouched         -> ("Related to changelist but not modified", dullColor Black)
      ModifiedProperties -> ("Files with modified properties", dullColor Yellow)
      Deleted            -> ("Deleted files", dullColor Red)
      Conflicted         -> ("Files with SVN conflict", dullColor Red)
      where
        dullColor :: Color -> SGR
        dullColor = SetColor Foreground Dull


showChanges :: Bool -> ChangesModel -> String
showChanges colored m = toPrintableString colored $ M.foldMapWithKey showChangeList m
  where
    showChangeList :: String -> ChangeList -> [StatusLine]
    showChangeList name (ChangeList cl) = let
        headerLines = map stringToLine $ case name of
          "" -> [ "Files not related to any changeslist:"
                ]
          n -> [ printf "Changelist '%s':" n
               , printf "  (use \"svn changelist '%s' <file>...\" to add files to this changelist)" n
               , printf "  (use \"svn changelist --remove <file>...\" to remove files from changelist)"
               , printf "  (use \"svn commit --cl '%s' -m <message> to commit this changelist)" n
               , printf ""
               ]
        content = M.foldMapWithKey showFilesGroup cl
        updateFirst :: (a -> a) -> [a] -> [a]
        updateFirst _ [] = []
        updateFirst f (x:xs) = f x:xs
      in case content of
        [] -> []
        someLines -> updateFirst (withConsoleIntensity BoldIntensity)
          headerLines ++ map (tabbed 2 ) someLines ++ [stringToLine ""]
          where
            withConsoleIntensity :: ConsoleIntensity -> StatusLine -> StatusLine
            withConsoleIntensity consoleIntensity = withStyle $ SetConsoleIntensity consoleIntensity
