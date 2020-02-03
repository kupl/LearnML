let rec max : int list -> int
=fun l -> match l with
| [] -> raise (Failure "error")
| [a] -> a
| hd::tl -> if hd > (max tl) then hd else max tl
 