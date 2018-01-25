(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
match lst with
| [] -> ([], [])
| (l1,l2)::tl ->
let a, b = unzip tl in l1::a, l2::b