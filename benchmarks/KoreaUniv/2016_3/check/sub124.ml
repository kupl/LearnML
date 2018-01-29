
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string
	  
	  let rec bound : exp -> var list 
	  = fun exp -> match exp with 
	  | V var -> []
	  | P (var, ex) -> var :: bound(ex)
	  | C (ex1, ex2) -> bound(ex1)@bound(ex2)
	  
	  let rec variables: exp -> var list
	  = fun exp -> match exp with 
	  | V var -> [var]
	  | P (var, ex) -> variables (ex)
	  | C (ex1, ex2) -> variables (ex1)@variables(ex2)
	  
	  let rec containHelper : var list * var -> bool 
	  = fun (bound, a) -> match (bound, a) with 
	  | ([], a) -> false
	  | (x :: tl, a) -> if x = a then true else containHelper(tl, a)
	  
	  let rec contain : var list * var list -> bool
	  = fun (bound, variables) -> match (bound, variables) with
	  | ([], variables) -> false
	  | (bound, []) -> true
	  | (bound, a::tl) -> if containHelper(bound, a) = false then false else contain (bound, tl)

	  let check : exp -> bool
	  = fun exp -> contain(bound(exp), variables(exp))
