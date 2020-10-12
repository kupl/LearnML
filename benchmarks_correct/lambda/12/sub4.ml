type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let check : lambda->bool = fun( met )->
  let rec checkName : string list*lambda -> bool = fun( l, m )->
	  match m with
		  P(id, mt)-> checkName( id::l, mt )
		  |V id -> List.mem id l
			|C(m1, m2) -> checkName(l, m1)&&checkName(l, m2) in
  checkName([], met)