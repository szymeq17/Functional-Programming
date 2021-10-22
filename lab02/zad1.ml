let length xs = List.fold_left (fun x y -> x+1) 0 xs;; 

let map f xs = List.fold_right (fun x y -> (f x) :: y) xs [];;

let append xs ys = List.fold_right (fun x y -> x :: y) xs ys;;

let append_rev xs ys = List.fold_left (fun x y -> y :: x) ys xs;;

let filter predicate xs = List.fold_right (fun x y -> if predicate x then x :: y else y) xs [];;

let rev_map f xs = List.fold_left (fun x y -> (f y) :: x) [] xs;;