
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec matchi : exp -> string list -> bool
  = fun exp lst -> 
  	match exp with
  	| V(a) -> 
  		begin
  		match lst with 
  			|[] -> false
  			|hd::tl ->if(hd = a) then true
  					 else matchi exp tl
  		end
  	| P(a,b) -> matchi b ([a]@lst)
  	| C(a,b) -> (matchi a lst)&&(matchi b lst)


  let rec check : exp -> bool
  = fun exp ->  
  	match exp with
  		| V(a) -> matchi exp []
  		| P(a,b) -> matchi b [a]
  		| C(a,b) -> (matchi a [])&&(matchi b [])
