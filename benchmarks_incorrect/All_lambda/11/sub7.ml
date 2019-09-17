(* hw 1_7. *)
type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda
and var = string
let check m =
	let rec areaList m1 =
		match m1 with
		 P(a, b) -> a::areaList b
		|_ -> [] in
	let rec stationList m2 =
		match m2 with
		 V a -> [a]
		|P(a, b) -> stationList b
		|C(a, b) -> stationList a @ stationList b in
	let rec searchArea al st =
		match al with
		 [] -> false
		|hd::tl ->
			(if hd = st then true
			 else searchArea tl st) in
	let rec matching al sl =
		match sl with
		 [] -> true
		|hd::tl ->
			(if searchArea al hd = false then false
			 else matching al tl) in
	let _ = areaList m in
	let _ = stationList m in
	matching (areaList m) (stationList m)

	(*
let _ =
	check (P("a", V "a"))
let _ =
	check (P("a", P("a", V "a")))
let _ =
	check (P("a", P("b", C(V "a", V "b"))))
let _ =
	check (P("a", C(V "a", P("b", V "a"))))
let _ =
	check (P("a", V "b"))
let _ =
	check (P("a", C(V "a", P("b", V "c"))))
let _ =
	check (P("a", P("b", C(V "a", V "c"))))
	*)
