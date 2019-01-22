
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
	 and var = string

	
	let rec lcheck: exp * var list -> bool
	= fun (exp, varList) ->
		match exp with
			| V(var1) -> (match varList with
										| [] -> false
										| hd :: tl -> if hd = var1 then true else lcheck(exp, tl))
			| P(var1, exp1) -> lcheck(exp1,  var1 :: varList)
			| C(exp1, exp2) -> lcheck(exp1, varList) && lcheck(exp2, varList)

	 

  let check : exp -> bool
  = fun exp -> lcheck(exp, [])
