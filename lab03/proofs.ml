open Logic

let p = Var "p";;
let q = Var "q";;
let r = Var "r";;

(* p → p *)
let proof1 = imp_i p (by_assumption p);;