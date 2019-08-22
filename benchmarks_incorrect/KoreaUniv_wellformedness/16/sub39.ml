
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string


	let rec check : lambda -> bool
	= fun lambda ->
		match lambda with
		| V (_) -> false
		| P (a, e) -> let l = cklist e in
								(match l with
								| [] -> true
								| hd :: tl -> if mtlist(a, l) then if remlist (a,l) = [] then true else
																										 (match e with
																										| V (_) -> true
																										| P (_,_) -> check e
																										| C (V (a), V (b)) -> true
																										| C (V (_), e1) -> check e1
																										| C (e1, V (_)) -> check e1
																										| C (e1, e2) -> check e)
															else false)
		| C (e1, e2) -> check e1 && check e2
	and cklist lambda =
		match lambda with
		| V (a) -> [a]
		| P (a, e) -> cklist e
		| C (e1, e2) -> cklist e1 @ cklist e2
	and mtlist (a, l) =
		match l with
		| [] -> false
		| hd :: tl -> if hd = a then true else mtlist (a, tl)
	and remlist (a, l) =
		match l with
		| [] -> []
		| hd :: tl -> if hd = a then remlist (a, tl) else [a] @ remlist (a, tl)

