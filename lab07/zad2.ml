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

let x = RS.bind (RS.random) (RS.random);;