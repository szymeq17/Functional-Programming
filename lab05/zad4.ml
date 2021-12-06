type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data =
  { prev : 'a dllist
  ; elem : 'a
  ; next : 'a dllist
  }

let prev xs = xs.prev
let elem xs = xs.elem
let next xs = xs.next

let create x = let rec list = lazy { prev = list; elem = x; next = list} in list
let create_from prev elem next = lazy { prev = prev; elem = elem; next = next}
let l2 = let rec list2 = lazy { prev = list1; elem = 2; next = list1} and list1 = lazy { prev = list2; elem = 1; next = list2} in list1

let of_list xs =
  match xs with
  | [] -> failwith "of_list"
  | x :: xs ->
    let rec start = create_from last x next
    and couple = aux start xs
    and next = fst couple
    and last = snd couple
    and aux before = function
      | [] -> start, before
      | x :: xs -> let rec current = lazy { prev = before; elem = x; next = after}
                    and couple = aux current xs
                    and after = fst couple
                    and last = snd couple 
                    in current, last
    in start