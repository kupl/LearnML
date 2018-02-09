(*********************)
(*     Problem 1     *)
(*********************)
let max1 a b = if a>=b then a else b
let min1 a b = if a<=b then a else b
let rec max : int list -> int
= fun lst -> match lst with
| [hd] -> hd
| hd :: tl -> max1 hd (max tl)

let rec min : int list -> int
= fun lst -> match lst with
| [hd] -> hd
| hd :: tl -> min1 hd (min tl)
