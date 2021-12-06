let for_all_cnt = ref 0
let mult_cnt = ref 0
let sorted_cnt = ref 0

let rec fold_left_cps f init xs cont = 
  match xs with
    | [] -> cont init
    | x :: xs -> f init x (fun y -> fold_left_cps f y xs cont)

(* -----------------------------------------------*)

let for_all pred xs = 
  fold_left_cps (fun x y cont -> 
    if x && pred y then (*let r = !for_all_cnt in for_all_cnt := r + 1;*) cont true else false)
    true 
    xs 
    (fun x -> x)

let pred x =
  if x mod 2 == 0 then true else false

let a = for_all pred [1; 2; 2; 2; 2]

(* -----------------------------------------------*)

let mult_list xs = 
  fold_left_cps (fun x y cont ->
    let res = x * y in if res == 0 then (*let r = !mult_cnt in mult_cnt := r + 1;*) 0 else cont res)
    1
    xs
    (fun x -> x)

let b = mult_list [1; 0; 3; 4; 5; 6]

(* -----------------------------------------------*)

let sorted xs =
  fold_left_cps (fun x y cont -> if y < x then false else (*let r = !sorted_cnt in sorted_cnt := r + 1;*) cont y)
  min_int
  xs
  (fun x -> true)

let c = sorted [1; 0; 3; 4; 5]