(*********************)
(*     Problem 1     *)
(*********************)
let rec max2 : int -> int -> int
= fun a b -> if a>=b then a else b
 
let rec max : int list -> int
= fun lst -> match lst with
| [] -> -999999999
| hd::tl -> max2 hd (max tl)
 