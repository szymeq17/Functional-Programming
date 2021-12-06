let f a b = a + b;;

let s n = n;;

let rec scan f a s = fun n -> if n == 0 then f a (s 0) else f (scan f a s (n - 1)) (s n);;

let newStream = scan f 0 s;; 