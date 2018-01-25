(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with
| [] -> ([],[])
| (a,b)::tl -> let (x,y) = unzip tl in (a::x, b::y)