
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

	let rec save : exp -> var list
	= fun exp ->
	match exp with
	|V a -> []
	|P (a, b) -> a :: []
	|C (a, b) -> save (a) @ save(b)

	let rec search : exp * var list -> bool
	= fun (exp, all) ->
	match exp  with
	V a -> begin match all with [] -> false |hd::tl -> if hd =  a then true else search (V a, tl) end
	|P(a, b) -> search (b, (a::all))
	|C(a, b) -> search (a, all) && search (b, all)

  let rec check : exp -> bool
  = fun exp ->
	match exp with
	|V a -> false
	|P (a, b) -> search(b, a::[])
	|C (a, b) -> check (a) && check (b)
