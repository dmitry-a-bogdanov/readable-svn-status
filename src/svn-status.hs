import Data.Functor
import System.Console.ANSI
import System.IO

import Parser
import StatusPrinter


main :: IO ()
main = do
    colored <- hSupportsANSIColor stdout
    getContents <&> parseFileLists <&> (showChanges colored) >>= putStr
    return ()
