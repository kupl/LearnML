  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  
	let rec change : (var * lambda) -> lambda
	= fun (v,e) -> match e with
	|V v' -> V v'
	|P(v', e') -> change (v', e')
	|C(e1, e2) -> if (V v) = e1 then (if (V v) = e2 then change(v, e1) 
		else change(v, e2)) else (if (V v) = e2 then change(v, e1) 
		else change(v, V v))

  let rec check : lambda -> bool
  =fun e -> match e with
	|V v -> false
	|P(v, e') -> if (V v) = (change (v, e')) then true else false
	|C(e1, e2) -> false
