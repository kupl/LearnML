
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

	let rec ch : (var list)*exp->bool
	= fun(l, exp) ->
		match exp with
		V v -> (match l with
								| hd::tl -> if(v=hd) then true else ch(tl, V v))
								| [] -> false
		| P (v, e) -> ch(l::v, e)
		| C (e1, e2) -> if (ch(l, e1) = true) && (ch(l, e2) = true) then true else false;;

	let check : exp -> bool
	= fun exp -> ch([],exp);;
