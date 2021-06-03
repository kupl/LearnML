
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

	let rec compare_var a b =
	match b with
	| [] -> false
	| hd::tl -> if hd = a then true else compare_var a tl

	let rec make_l lambda l =
	match lambda with
	| V (v) -> compare_var v l
	| P (v, e) -> make_l e (l@[v])
	| C (e1, e2) -> (make_l e1 l)&&(make_l e2 l)

  let check : lambda -> bool
  = fun lambda ->
	make_l lambda [];;
