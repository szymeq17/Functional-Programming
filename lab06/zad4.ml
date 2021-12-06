let rec fold_left_cps f init xs cont = 
  match xs with
    | [] -> cont init
    | x :: xs -> f init x (fun y -> fold_left_cps f y xs cont)


let fold_left f init xs = fold_left_cps (fun x y cont -> cont (f x y)) init xs (fun x -> x)

let length xs = fold_left (fun x y -> x+1) 0 xs
let sum xs = fold_left (fun x y -> x + y) 0 xs