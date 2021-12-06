type 'a zlist = Zip of 'a list * 'a list

let of_list xs = Zip([], xs)

let to_list zip_list = 
  match zip_list with
  | Zip(context, list) -> List.rev_append context list

let elem zip_list = 
  match zip_list with
  | Zip(_, []) -> None
  | Zip(_, list) -> List.hd list

let move_left zip_list =
  match zip_list with
  | Zip([], _) -> failwith "No items on the left!"
  | Zip(x :: xs, list) -> Zip(xs, x :: list)

let move_right zip_list =
  match zip_list with
  | Zip(_, []) -> failwith "No items on the right!"
  | Zip(list, x :: xs) -> Zip(x :: list, xs)

let insert elem (Zip(context, list)) = Zip(elem :: context, list)

let remove zip_list =
  match zip_list with
  | Zip([], _) -> failwith "No item behind cursor!"
  | Zip(x :: xs, list) -> Zip(xs, list)