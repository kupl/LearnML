type lambda = V of var
					| P of var * lambda
					| C of lambda * lambda
and var = string

let rec check m =
	let rec checkArea (lst, n) =
		match lst with
		| [] -> false
		| hd::tl ->
			if (hd = n) then (true)
			else (checkArea(tl, n))
	in
	let rec add_env (env, n) =
		match env with
		| [] -> [n]
		| hd::tl ->
			if (hd = n) then (env)
			else hd::(add_env (tl, n))
	in
	let rec my_check (env, m) =
		match m with
		| V n -> checkArea (env, n)
		| P (n, m) -> my_check (add_env (env, n), m)
		| C (m1, m2) -> (my_check (env, m1)) && (my_check (env, m2))
	in
	my_check ([], m)

