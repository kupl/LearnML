
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

	let rec mem : var *  var list -> bool
	= fun (key, lst) -> match lst with
	| [] -> false
	| hd::tl -> if hd = key then true else false||mem(key, tl)

  let check : exp -> bool
  = fun exp ->
	let rec ch : exp * var list-> bool
	= fun(exp, lst) -> match exp with
	| V(x) -> mem(x, lst)
	| P(x, p) -> ch(p, x::lst)
	| C(p, q) -> ch(p, lst)&&ch(q, lst)
	in ch(exp, [])
