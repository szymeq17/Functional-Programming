let rec tabulate ?(a=0) b s = if a > b then [] else (s a) :: (tabulate ~a:(a+1) b s);;

let s n = n * n;;
tabulate ~a:1 10 s;;