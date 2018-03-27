(* exception *)
exception Improper_input

let rec max : int list -> int
=fun l -> 
match l with
| [] -> raise Improper_input
| [hd] -> hd
| hd::tl -> if hd > (max tl) then hd else (max tl)

let rec min : int list -> int
=fun l -> 
match l with
| [] -> raise Improper_input
| [hd] -> hd
| hd::tl -> if hd < (min tl) then hd else (min tl)
