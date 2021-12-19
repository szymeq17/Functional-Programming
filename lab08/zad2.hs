import System.IO (isEOF)
import Data.Char

data StreamTrans i o a 
    = Return a
    | ReadS (Maybe i -> StreamTrans i o a)
    | WriteS o (StreamTrans i o a)

runIOStreamTrans :: StreamTrans Char Char a -> IO a
runIOStreamTrans (Return a) = return a
runIOStreamTrans (ReadS cont) = do
    done <- isEOF
    if done
        then runIOStreamTrans $ cont $ Nothing
        else do
            inp <- getChar
            runIOStreamTrans $ cont $ Just inp
    
runIOStreamTrans (WriteS x c) = do
    putChar x
    runIOStreamTrans c

toLowerStream :: StreamTrans Char Char String
toLowerStream = ReadS (\ i -> case i of
                            Nothing -> Return "Bye!"
                            Just x  -> WriteS (toLower x) toLowerStream)
                
main = runIOStreamTrans toLowerStream