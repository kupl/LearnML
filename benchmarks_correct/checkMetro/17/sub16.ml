type metro = STATION of name
					| AREA of name * metro
					| CONNECT of metro * metro
and name = string

let rec checkMetro m =
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
		| STATION n -> checkArea (env, n)
		| AREA (n, m) -> my_check (add_env (env, n), m)
		| CONNECT (m1, m2) -> (my_check (env, m1)) && (my_check (env, m2))
	in
	my_check ([], m)

