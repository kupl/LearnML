type exp = 
	V of var 
	| P of var * exp 
	| C of exp * exp 
and var = string 

let rec evals a t = 
	match t with
	[] -> false
	| hd::tl -> if hd = a then true else evals a tl

let rec div a t =
	match a with
	V a -> evals a t
	| P (a,b) -> div b (a::t)
	| C (a,b) -> (div a t) && (div b t)

let chk a = div a []
