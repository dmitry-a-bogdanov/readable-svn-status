import System.Process
import System.IO
import Parser
import Data.Functor
import Data.List


main :: IO ()
main = do
    (_, Just outStream, Just errStream, pHandle) <- createProcess (proc "svn" ["status"]) { std_out = CreatePipe,
        std_err = CreatePipe }
    --hGetContents outStream <&> (readModel :: String -> NoChangelistModel) <&> toString >>= putStrLn
    putStrLn $ show ("--- Changelist " `isPrefixOf` "--- Changelist 'L1':")
    putStrLn $ show (read "--- Changelist 'L1':" :: SvnStatusLine)
    hGetContents outStream <&> (readList :: ReadS [SvnStatusLine]) <&> show >>= putStrLn
    return ()