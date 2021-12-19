import System.IO (isEOF)
import Data.Char

main = echoLower

echoLower :: IO ()
echoLower = do 
    done <- isEOF
    if done
        then putStrLn "Bye!"
        else do 
            inp <- getChar
            putChar $ toLower inp
            echoLower