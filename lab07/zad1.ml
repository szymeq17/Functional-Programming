module type RandomMonad = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val random : int t

end

module RS: sig include RandomMonad 
  val run : int -> 'a t -> 'a
end = struct
  type 'a t = int -> 'a * int

  let return x s = (x, s)

  let bind m f s = let (x, s) = m s in f x s 

  let random a =
    let b = 16807 * (a mod 127773) - 2836 * (a / 127773) in 
    let res = if b > 0 then  b else b + 2147483647 in
    (res, res)

  let run s m = fst (m s)
end

let (let* ) = RS.bind

let delete k xs = 
  let rec del k xs acc =  if    k = 0
                          then  RS.return (acc @ (List.tl xs))
                          else  let* head = RS.return (List.hd xs) in
                                let* tail = RS.return (List.tl xs) in
                                let* acc = RS.return (head :: acc) in
                                del (k - 1) tail acc
  in del k xs []

let rec kth k xs =  if k = 0
                    then RS.return (List.hd xs)
                    else kth (k - 1) (List.tl xs)

let rec shuffle xs = 
  match xs with
  | []    ->  RS.return []
  | xs    ->  let* k = RS.random in
              let* k = RS.return (k mod (List.length xs)) in
              let* suff = delete k xs in
              let* suff = shuffle suff in
              let* elem = kth k xs in
              RS.return (elem :: suff)