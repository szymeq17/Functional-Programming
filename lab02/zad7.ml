type 'a tree = 
  | Leaf
  | Node of int * 'a * 'a tree * 'a tree;;

exception Empty;;

let height t = 
  match t with
    | Leaf -> 0
    | Node(h, _, _, _) -> h;;


let order t1 t2 = 
  match (t1, t2) with
    | (Leaf, _) -> (t2, t1)
    | (_, Leaf) -> (t1, t2)
    | (Node(h1, _, _, _), Node(h2, _, _, _)) -> 
      if h1 > h2 then (t1, t2) else (t2, t1);;

let min_height t1 t2 =
  match (t1, t2) with
    | (Leaf, _) -> height t2
    | (_, Leaf) -> height t1
    | (Node(h1, _, _, _), Node(h2, _, _, _)) ->
      if height t1 < height t2 then height t1 else height t2;;


let join t1 t2 = 
  let rec join_helper t1 t2 =
    match (t1, t2) with
      | (Leaf, _) -> t2
      | (_, Leaf) -> t1
      | (Node(h1, value1, l1, r1), Node(h2, value2, l2, r2)) ->
        if value1 < value2 then let subtree = join_helper r1 t2
          in Node((min_height subtree l1) + 1, value1, fst (order l1 subtree), snd (order l1 subtree))
        else let subtree = join_helper r2 t1
          in Node((min_height subtree l2) + 1, value2, fst (order l2 subtree), snd (order l2 subtree))
  in join_helper t1 t2;; 
    

let add value t = join t (Node(1, value, Leaf, Leaf));;

let delete_min t  = 
  match t with
    | Leaf -> raise Empty
    | Node(_, _, l, r) -> join l r;;

let get_min t = 
  match t with
    | Leaf -> raise Empty
    | Node(_, value, _, _) -> value;;

let create_node value l r = Node(height r, value, l, r);;


