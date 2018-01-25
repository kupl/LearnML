(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
match lst with
| [] -> ([],[])
| (r1, r2)::tl ->
match unzip tl with
| (l1, l2) -> (r1::l1, r2::l2)
