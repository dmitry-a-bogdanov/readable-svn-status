import System.Process
import System.IO
import Parser
import Data.Functor


main :: IO ()
main = do
    (_, Just outStream, Just errStream, pHandle) <- createProcess (proc "svn" ["status"]) { std_out = CreatePipe,
        std_err = CreatePipe }
    hGetContents outStream <&> (readModel :: String -> NoChangelistModel) <&> toString >>= putStrLn
    return ()
