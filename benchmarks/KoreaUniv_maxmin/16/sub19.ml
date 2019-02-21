(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with (* TODO *)
| [] -> 0
| x::[] -> x
| hd::tl -> if hd > (max tl) then hd else max tl

let rec min : int list -> int
= fun lst -> match lst with (* TODO *)
| [] -> 0
| x::[] -> x
| hd::tl -> if hd < (min tl) then hd else min tl
