let distribute c l =
  let rec insert acc1 acc2 xs = 
    match xs with 
      | [] -> acc2
      | hd::tl ->
        insert (hd::acc1) ((List.rev_append acc1 (hd::c::tl)) :: acc2) tl
  in 
  insert [] [c::l] l;;


let rec permutations xs  = 
  match xs with
    | [] -> [[]]
    | x :: xs -> List.fold_left (fun acc y -> List.append (distribute x y) acc) [] (permutations xs);;


let rm x xs = 
  let rec aux x xs acc = 
    match xs with
      | [] -> acc
      | y :: ys -> if x <> y then aux x ys (acc @ [y]) else acc @ ys
  in aux x xs [];;

let rec permutationsSelect xs =
  match xs with
    | [] -> []
    | x :: [] -> [[x]]
    | xs -> List.fold_left (fun acc x -> acc @ List.map (fun p -> x :: p) (permutations (rm x xs))) [] xs;;