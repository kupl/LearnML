let rec max : int list -> int
=fun l -> match l with
[] -> min_int
| hd::tl -> if hd > max tl then hd else max tl;;
 