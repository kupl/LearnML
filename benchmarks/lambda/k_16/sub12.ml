
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec findvariable : var -> var list -> bool
  = fun v l ->
  	match l with
  		| [] -> false
  		| hd::tl -> if hd = v then true else findvariable v tl

  let rec checkcheck : lambda -> var list -> bool
  = fun lambda l ->
  	match lambda with
  		| V a ->
  			findvariable a l
  		| P (a, b) ->
  			checkcheck b (l@[a])
  		| C (a, b) ->
  			(checkcheck a l)&&(checkcheck b l)

  let check : lambda -> bool
  = fun lambda ->
  	checkcheck lambda []
