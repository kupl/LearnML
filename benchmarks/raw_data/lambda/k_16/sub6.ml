
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
	 and var = string

	
	let rec lcheck: lambda * var list -> bool
	= fun (lambda, varList) ->
		match lambda with
			| V(var1) -> (match varList with
										| [] -> false
										| hd :: tl -> if hd = var1 then true else lcheck(lambda, tl))
			| P(var1, lambda1) -> lcheck(lambda1,  var1 :: varList)
			| C(lambda1, lambda2) -> lcheck(lambda1, varList) && lcheck(lambda2, varList)

	 

  let check : lambda -> bool
  = fun lambda -> lcheck(lambda, [])
