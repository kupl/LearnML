type lambda = V of var
| P of var * lambda
| C of lambda * lambda

and var = string

let rec inlist a l = 
match l with
| [] -> false
| h::t -> if a = h then true
	else (inlist a t)

let rec checkm l m =
match m with
| V a -> (inlist a l)
| P (a, b) -> (checkm (a::l) b)
| C (a, b) -> (checkm l a) && (checkm l b)

let rec check m = 
(checkm [] m)
