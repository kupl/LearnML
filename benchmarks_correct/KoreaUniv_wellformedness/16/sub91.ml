
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let check : lambda -> bool
  = fun lambda ->

	let rec evaluate lambdaression enviornment = match lambdaression with
		|V v ->
			let rec checker lst var = match lst with
				|[] -> false
				|h::t -> if h = var then true else (checker t var)
			in
			if (checker enviornment v = true) then true else false
		|P (v, e) -> if (evaluate e (enviornment @ [v]) = true) then true else false
		|C (e1, e2) -> if (evaluate e1 enviornment && evaluate e2 enviornment = true) then true else false
	in
	
	evaluate lambda []
