type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string


let check t =
	let rec getid t r = 
	match t with
	P(t1, t2) -> getid t2 (t1::r)
	|C(t1, t2) -> (getid t1 r) && (getid t2 r)
	|V t1 -> List.exists (fun x -> if x = t1 then true else false) r
	in

	getid t []
