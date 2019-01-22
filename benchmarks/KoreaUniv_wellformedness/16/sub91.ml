
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let check : exp -> bool
  = fun exp ->

	let rec evaluate expression enviornment = match expression with
		|V v ->
			let rec checker lst var = match lst with
				|[] -> false
				|h::t -> if h = var then true else (checker t var)
			in
			if (checker enviornment v = true) then true else false
		|P (v, e) -> if (evaluate e (enviornment @ [v]) = true) then true else false
		|C (e1, e2) -> if (evaluate e1 enviornment && evaluate e2 enviornment = true) then true else false
	in
	
	evaluate exp []
