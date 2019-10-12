import System.Console.ANSI
import System.IO

import Parser
import StatusPrinter


main :: IO ()
main = do
    colored <- hSupportsANSIColor stdout
    content <- getContents
    case parseModel content of
      Right output -> putStr $ showChanges colored output
      Left e -> print e
    return ()
