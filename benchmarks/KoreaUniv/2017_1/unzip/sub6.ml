(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
let first_element tup = match tup with 
| (a,b)->a
in
let second_element tup = match tup with 
| (a,b)->b
in
match lst with
| [] -> ([],[])
| hd::tl -> ((first_element hd::first_element(unzip tl)), (second_element hd::second_element(unzip tl)));;
