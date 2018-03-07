
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

	let rec compare_var a b =
	match b with
	| [] -> false
	| hd::tl -> if hd = a then true else compare_var a tl

	let rec make_l exp l =
	match exp with
	| V (v) -> compare_var v l
	| P (v, e) -> make_l e (l@[v])
	| C (e1, e2) -> (make_l e1 l)&&(make_l e2 l)

  let check : exp -> bool
  = fun exp ->
	make_l exp [];;
