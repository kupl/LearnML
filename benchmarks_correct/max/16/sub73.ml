let rec max : int list -> int
=fun l -> match l with
  [] -> 0
  |[x] -> x
  |hd::tl -> if hd >= max tl then hd else max tl;;
 