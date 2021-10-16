let hd s = s 0;;
let tl s = fun n -> s n+1;; 
let add s i = fun n -> (s n) + i;;
let map s f = fun n -> f (s n);;
let map2 f s1 s2 = fun n -> f (s1 n) (s2 n);;
let replace n a s = fun m -> if m == n then a else s m;;
let take_every s n = fun m -> s n*m;;

let simpleStream n = n;;
let squareStream n = n * n;;

hd simpleStream;;
(tl simpleStream) 0;;
(add squareStream 2) 5;;
let cube n = n * n * n;;
(map simpleStream cube) 4;;
(map2 (+) simpleStream squareStream) 2;;
(replace 5 100 simpleStream) 5;;
(replace 5 100 simpleStream) 1;;
(take_every squareStream 2) 1;;
