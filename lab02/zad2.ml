let rec sublists xs = 
  match xs with
    | [] -> [[]]
    | x :: xs -> let sl = sublists xs in
                  (List.map (fun list -> x :: list) sl) @ sl;;    