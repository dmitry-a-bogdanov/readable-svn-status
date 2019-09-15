module StatusPrinter where

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


withStyle :: TextStyle -> String -> String
withStyle style text = (getString Escape) <> (getString style) <> text <> (getString Escape) <> (getString Reset)
