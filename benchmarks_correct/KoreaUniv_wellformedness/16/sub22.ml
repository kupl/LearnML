
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec eval : lambda -> var list -> bool
  = fun lambda lst ->
  	match lambda with
  	| V var -> 
	  	begin
	  		match lst with
	  		|[] -> false
	  		|hd::tl -> if var = hd then true else eval lambda tl
	  	end
	| P (var, lambda2) -> eval lambda2 (var::lst)
	| C (lambda1, lambda2) -> (eval lambda1 lst)&&(eval lambda2 lst)


  let check : lambda -> bool
  = fun lambda -> 
  	let state = [] in
  		match lambda with
  		| V var -> eval lambda state
  		| P (var, lambda) -> eval lambda (var::state)
  		| C (lambda1, lambda2) -> (eval lambda1 state)&&(eval lambda2 state)
