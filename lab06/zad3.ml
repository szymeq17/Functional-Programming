exception End

let for_all_cnt = ref 0
let mult_cnt = ref 0
let sorted_cnt = ref 0

(* -----------------------------------------------*)

let and_with_ex pred x y = 
  let r = !for_all_cnt in for_all_cnt := r + 1;
  if x && (pred y) then true else raise End


let for_all predicate xs = 
  try List.fold_left (and_with_ex predicate) true xs with End -> false

let pred x =
  if x mod 2 == 0 then true else false

let a = for_all pred [2; 2; 2; 1; 2];;

(* -----------------------------------------------*)

let mult_with_ex x y =
  let r = !mult_cnt in mult_cnt := r + 1;
  let res = x * y in 
    if res == 0 then raise End else res

let mult_list xs = try List.fold_left mult_with_ex 1 xs with End -> 0

let b = mult_list [2; 2; 2; 0; 2; 2; 2]

(* -----------------------------------------------*)

let less_eq_with_ex x y =
  let r = !sorted_cnt in sorted_cnt := r + 1;
  if x > y then raise End else y

let sorted xs = 
  try 
    let _ = List.fold_left less_eq_with_ex min_int xs
      in true 
  with End -> false

let c = sorted [1; 2; 3; 0; 4; 5; 6]