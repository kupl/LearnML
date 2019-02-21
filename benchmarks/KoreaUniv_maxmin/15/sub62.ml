let rec max : int list -> int
=fun l -> 
match l with
[] -> raise (Failure "list is too short")
| h::t->if t=[] then h else if h > max t then h else max t;;


let rec min : int list -> int
=fun l -> 
match l with
[] -> raise (Failure "list is too short")
| h::t->if t=[] then h else if h < max t then h else max t;;
