
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

let rec forcheck 
=fun (ex, l) -> match ex with
	|V v-> (match l with []->false
											|hd::tl -> if v=hd then true else forcheck(V v, tl))
	|P(v, e1) -> forcheck(e1, v::l)
	|C(e1, e2) -> if forcheck(e1, l)=true && forcheck(e2, l)=true then true else false

  let check : lambda -> bool
  = fun lambda -> forcheck(lambda,[])
