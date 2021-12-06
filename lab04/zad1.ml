type 'a nlist =
| Nil
| Zero of ('a * 'a) nlist
| One  of 'a * ('a * 'a) nlist

let rec cons : 'a. 'a -> 'a nlist -> 'a nlist =
  fun x xs ->
  match xs with
  | Nil        -> One(x, Nil)
  | Zero xs    -> One(x, xs)
  | One(y, xs) -> Zero(cons (x, y) xs)

let rec view : 'a. 'a nlist -> ('a * 'a nlist) option =
  function
  | Nil -> None
  | Zero xs    ->
    begin match view xs with
    | None -> None
    | Some((x, y), xs) -> Some(x, One(y, xs))
    end
  | One(x, xs) -> Some (x, Zero xs)

let hd xs = 
  match (view xs) with
    | Some (x, _) -> x

let tl xs = 
  match (view xs) with
    | Some (_, xs) -> xs

let rec nth : 'a. 'a nlist -> int -> 'a =
  fun xs n ->
  match xs with
  | Nil -> raise Not_found
  | Zero xs ->
    let (x, y) = nth xs (n / 2) in
    if n mod 2 = 0 then x
    else y
  | One(x, xs) ->
    if n = 0 then x
    else nth (Zero xs) (n-1)