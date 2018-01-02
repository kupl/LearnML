type var = string

type exp =
  | V of var
  | P of var * exp
  | C of exp * exp

let rec cklist exp =
	match exp with
	| V (a) -> [a]
	| P (a, e) -> cklist e
	| C (e1, e2) -> cklist e1 @ cklist e2

let rec mtlist (a, l) =
	match l with
	| [] -> false
	| hd :: tl -> if hd = a then true else mtlist (a, tl)
	
let rec remlist (a, l) =
	match l with
	| [] -> []
	| hd :: tl -> if hd = a then remlist (a, tl) else [a] @ remlist (a, tl)

let rec check : exp -> bool
= fun exp ->
	match exp with
	| V (_) -> false
	| P (a, e) -> 
		let l = cklist e in
			(match l with
			| [] -> true
			| hd :: tl -> 
				if mtlist(a, l) then 
					if remlist (a,l) = [] then true else
					 (match e with
					| V (_) -> true
					| P (_,_) -> check e
					| C (V (a), V (b)) -> true
					| C (V (_), e1) -> check e1
					| C (e1, V (_)) -> check e1
					| C (e1, e2) -> check e)
				else false)
	| C (e1, e2) -> check e1 && check e2
