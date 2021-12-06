 let cnt = ref 0

 let get_counter =
  let next = ref 0 in
    let next () =
      let n = !next in
        next := n + 1;
        n
    and reset () = 
      next := 0
    in (next, reset)

let cntr = get_counter
let next = fst cntr
let reset = snd cntr