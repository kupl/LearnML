(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
[a] -> a
| hd::tl -> if hd > max tl then hd else max tl 
| [] -> 0

let rec min : int list -> int
= fun lst -> match lst with
[a] -> a
| hd::tl -> if hd < min tl then hd else min tl
| [] -> 0
