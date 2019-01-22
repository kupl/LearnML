
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec eval : exp -> var list -> bool
  = fun exp lst ->
  	match exp with
  	| V var -> 
	  	begin
	  		match lst with
	  		|[] -> false
	  		|hd::tl -> if var = hd then true else eval exp tl
	  	end
	| P (var, exp2) -> eval exp2 (var::lst)
	| C (exp1, exp2) -> (eval exp1 lst)&&(eval exp2 lst)


  let check : exp -> bool
  = fun exp -> 
  	let state = [] in
  		match exp with
  		| V var -> eval exp state
  		| P (var, exp) -> eval exp (var::state)
  		| C (exp1, exp2) -> (eval exp1 state)&&(eval exp2 state)
