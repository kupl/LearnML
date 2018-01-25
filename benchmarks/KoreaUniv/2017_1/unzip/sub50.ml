(* problem 7*)
let rec decompose_a l =
match l with
| [] -> []
| (a,_)::tl -> [a]@(decompose_a tl);;

let rec decompose_b l =
match l with
| [] -> []
| (_,b)::tl -> [b]@(decompose_b tl);;

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
match lst with
| [] -> ([], [])
| (a,b)::tl -> ([a]@(decompose_a tl), [b]@(decompose_b tl));;