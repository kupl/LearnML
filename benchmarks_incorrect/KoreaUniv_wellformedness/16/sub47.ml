
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec check : lambda -> bool
  = fun lambda -> 
			match lambda with	
			|V _ -> false
			|C(_,_) -> false
			|P(v1, V v2)-> if v1=v2 then true else false
			|P(v1, P (v2, e1))-> if v1=v2 && check (P(v1,e1)) then true else false
			|P(v1, C (e1, e2))-> check (P(v1,e1)) && check (P(v1,e2))

