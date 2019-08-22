  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  
  let rec check : lambda -> bool
  = fun e -> true

	let rec check_2 : lambda*(var list) -> bool
	= fun (e,l) ->
	match e with
	V v ->
		(match l with
		[] -> false
	|	h::t -> if v=h then true else check_2 (V v,t))
| P (v,e1) -> check_2 (e1, v::l)
| C (e1,e2) -> if check_2 (e1,l)=true && check_2 (e2,l)=true then true
								else false;;

	let check : lambda -> bool
	= fun e -> check_2 (e,[]);;
