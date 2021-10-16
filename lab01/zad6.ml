let ctrue x y = if true then x else y;;
let cfalse x y = if false then x else y;;

let cand p q = p (q ctrue cfalse) cfalse;;
let cor p q = p ctrue (q ctrue cfalse);;

let cbool_of_bool p = if p then ctrue else cfalse;;
let bool_of_cbool cbool = cbool true false;;




