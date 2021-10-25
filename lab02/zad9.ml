type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z };;

let cnil = { clist = fun f x -> x};;

let ccons a list = { clist = fun f x -> f a (list.clist f x) };;

let map mapper the_clist = 
  the_clist.clist (fun a list -> ccons (mapper a) list) cnil;;

let append clist1 clist2 = { clist = fun f x -> 
  clist1.clist f (clist2.clist f x) };;

let clist_to_list the_clist = the_clist.clist (fun a list -> a :: list) [];;
  
let rec clist_of_list xs = 
  match xs with
  | [] -> cnil
  | x :: xs -> ccons x (clist_of_list xs);;
    
let prod clist1 clist2 = { clist = fun f x -> clist1.clist (clist2.clist f) x };;