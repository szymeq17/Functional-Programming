let f1 = fun a -> fun b -> fun c -> a (b c);;
let f2 = fun a -> fun b -> a;; 
let f3 = fun a -> fun a -> a;;

