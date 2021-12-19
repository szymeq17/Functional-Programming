import Data.Char

data StreamTrans i o a 
    = Return a
    | ReadS (Maybe i -> StreamTrans i o a)
    | WriteS o (StreamTrans i o a)

listTrans :: StreamTrans i o a -> [i] -> ([o], a)
listTrans streamTrans xs = memListTrans streamTrans xs []

memListTrans :: StreamTrans i o a -> [i] -> [o] -> ([o], a)
memListTrans (Return a) xs mem = (mem, a)
memListTrans (ReadS cont) xs mem =
    case xs of
        []            -> memListTrans (cont Nothing) [] mem
        lhead : ltail -> memListTrans (cont $ Just lhead) ltail mem
memListTrans (WriteS x c) xs mem = memListTrans c xs (mem ++ [x]) 

toLowerStream :: StreamTrans Char Char String
toLowerStream = ReadS (\ i -> case i of
                            Nothing -> Return "Bye!"
                            Just x  -> WriteS (toLower x) toLowerStream)

toSquareStream :: StreamTrans Int Int String
toSquareStream = ReadS (\ i -> case i of
                            Nothing -> Return "Bye!"
                            Just x  -> WriteS (x*x) toSquareStream)

main = putStrLn $ show $ take 3 $ fst $ listTrans toSquareStream [1..]
