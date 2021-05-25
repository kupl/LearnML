(* Problem 1: filter *) (*********************) 

let rec filter f l =
match l with
[] -> []
| h::t ->
if (f h) = true then h::(filter f t)
else filter f t;;
