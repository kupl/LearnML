(*********************)
(*     Problem 1     *)
(*********************)
let big a b = if a > b then a else b

let rec max : int list -> int
= fun lst ->
match lst with
|[]->(-10000000000)
|hd::tl->big hd (max tl);;

let small a b = if a < b then a else b

let rec min : int list -> int
= fun lst ->
match lst with 
|[]->10000000000
|hd::tl-> small hd (min tl)
