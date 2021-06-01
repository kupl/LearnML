(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
[] -> -100000
| hd::tl -> if hd > max(tl) then hd else max(tl);;  
