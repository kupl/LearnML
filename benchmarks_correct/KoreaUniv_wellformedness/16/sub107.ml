
	type lambda =
	| V of var
	| P of var * lambda
	| C of lambda * lambda
	and var = string

let rec check2 : lambda*(var list) -> bool
= fun (e,l) ->
	match e with
	|V v ->
		(match l with
		|[] -> false
		|hd::tl -> if v = hd then true else check2(V v, tl))
	|P(v, e1) -> check2(e1, v::l)
	|C(e1, e2)-> if check2(e1,l) = true && check2(e2,l) = true then true
		else false

let rec check : lambda -> bool
= fun lambda -> check2(lambda, [])
