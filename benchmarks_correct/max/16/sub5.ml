(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> let retnmax : int -> int -> int = fun a b -> if a>b then a else b in match lst with [a] -> a | hd::tl -> retnmax hd (max tl);;
 