let sufixes xs = accumulate (List.tl) [] xs;;

let prefixes xs = List.rev (accumulate (without_last) [] xs);;

let rec accumulate f result xs = if length xs == 0 then [] :: result else xs :: accumulate f result (f xs);; 

let rec accumulatePref result xs = if length xs == 0 then 

let rec without_last xs = if length xs == 1 then [] else (List.hd xs) :: (without_last (List.tl xs));; 