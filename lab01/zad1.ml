let identity = fun a -> a + 0;;

let f1 = fun a -> fun b -> fun c -> a (b c);;
let compose f g = fun x -> f (g x);;
let f2 = fun a -> fun b -> a;; 
let f3 = fun x y -> if true then x else y;;

