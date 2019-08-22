
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

	let rec mem : var *  var list -> bool
	= fun (key, lst) -> match lst with
	| [] -> false
	| hd::tl -> if hd = key then true else false||mem(key, tl)

  let check : lambda -> bool
  = fun lambda ->
	let rec ch : lambda * var list-> bool
	= fun(lambda, lst) -> match lambda with
	| V(x) -> mem(x, lst)
	| P(x, p) -> ch(p, x::lst)
	| C(p, q) -> ch(p, lst)&&ch(q, lst)
	in ch(lambda, [])
