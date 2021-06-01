(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
| [] -> 0
| hd::tl -> if hd > (max tl) then hd else (max tl)