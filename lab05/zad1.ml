let rec fix f x = f (fix f) x

let fib_f fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2)


let rec fix_with_limit max_depth f x =
  if max_depth == 0 then
     failwith "Max depth reached!" 
  else 
    f (fix_with_limit (max_depth - 1) f) x

let fib = fix_with_limit 5 fib_f

let memory = Hashtbl.create 10

let rec fix_memo f x = 
  if Hashtbl.mem memory x == true then
    Hashtbl.find memory x
  else
    let res = f (fix_memo f) x in 
      Hashtbl.add memory x res;
      res
    
let fib_mem = fix_memo fib_f

