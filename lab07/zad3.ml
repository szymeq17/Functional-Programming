module IdMonad : sig
  type 'a t
  
  val return : 'a -> 'a t
  
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val run : 'a t -> 'a
end = struct
  type 'a t = 'a

  let return x = x

  let bind m f = f m

  let run x = x
end

module DefferedMonad: sig
  type 'a t

  val return: 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val run : 'a t -> 'a
end = struct
  type 'a t = unit -> 'a

  let return x = fun () -> x

  let bind m f = f (m ())

  let run x = x()
end
