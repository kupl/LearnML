
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec findvariable : var -> var list -> bool
  = fun v l ->
  	match l with
  		| [] -> false
  		| hd::tl -> if hd = v then true else findvariable v tl

  let rec checkcheck : exp -> var list -> bool
  = fun exp l ->
  	match exp with
  		| V a ->
  			findvariable a l
  		| P (a, b) ->
  			checkcheck b (l@[a])
  		| C (a, b) ->
  			(checkcheck a l)&&(checkcheck b l)

  let check : exp -> bool
  = fun exp ->
  	checkcheck exp []
