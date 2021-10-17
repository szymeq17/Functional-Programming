let zero f x = if true then x else f x;;

let succ number f x = f (number f x);;

let add number1 number2 f x = number1 f (number2 f x);;

let mul number1 number2 f x = number1 (number2 f) x;;

let int_of_cnum cnum = cnum (fun a -> a+1) 0;;

let rec cnum_of_int n = if n == 0 then zero else succ (cnum_of_int (n-1));;

let is_zero number = number (fun x -> cfalse) ctrue;;

let num1 = (add (succ zero) (succ (succ zero)));;
let num2 = mul (succ (succ (succ zero))) (succ (succ zero));;