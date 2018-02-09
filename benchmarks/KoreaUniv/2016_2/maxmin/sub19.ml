(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with (* TODO *)
| [] -> 0
| x::[] -> x
| hd::tl when hd>(max tl) -> hd
| hd::tl ->(max tl) 

let rec min : int list -> int
= fun lst -> match lst with (* TODO *)
| [] -> 0
| x::[] -> x
| hd::tl when hd<(min tl) -> hd
| hd::tl -> (min tl)