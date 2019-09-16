import Data.Functor

import Parser
import StatusPrinter


main :: IO ()
main = do
    getContents <&> parseFileLists <&> showChanges >>= putStr
    return ()
