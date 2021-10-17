(* -------------------------BOOLEANS------------------------- *)
type cbool = { cbool: 'a. 'a -> 'a -> 'a };;

let ctrue = { cbool = fun x y -> x };;
let cfalse = { cbool = fun x y -> y };;

let cand p q = p.cbool (q.cbool ctrue cfalse) cfalse;;
let cor p q = p.cbool ctrue (q.cbool ctrue cfalse);;

let cbool_of_bool boolean = if boolean then ctrue else cfalse;;
let bool_of_cbool theCbool = theCbool.cbool true false;; 
(* ----------------------------------------------------------- *)

(* --------------------------NUMBERS-------------------------- *)
type cnum = { cnum: 'a. ('a -> 'a) -> 'a -> 'a };;

let zero = { cnum = fun f x -> x };;

let succ theCnum = { cnum = fun f x -> f (theCnum.cnum f x) };;

let add cnum1 cnum2 = {cnum = fun f x -> cnum1.cnum f (cnum2.cnum f x) };;

let mul cnum1 cnum2 = {cnum = fun f x -> cnum1.cnum (cnum2.cnum f) x };;

let int_of_cnum theCnum = theCnum.cnum (fun x -> x + 1) 0;;

let rec cnum_of_int n = if n == 0 then zero else succ (cnum_of_int (n - 1));;

let is_zero theCnum = theCnum.cnum (fun x -> cfalse) ctrue;;