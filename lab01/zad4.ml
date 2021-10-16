let f a b = a + b;;

let rec scan f a s = fun n -> if n == 0 then f a (s 0) else f (scan f a s) (s n);; 
(f (f a (s 0)) (s 1))