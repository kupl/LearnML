
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec matchi : lambda -> string list -> bool
  = fun lambda lst -> 
  	match lambda with
  	| V(a) -> 
  		begin
  		match lst with 
  			|[] -> false
  			|hd::tl ->if(hd = a) then true
  					 else matchi lambda tl
  		end
  	| P(a,b) -> matchi b ([a]@lst)
  	| C(a,b) -> (matchi a lst)&&(matchi b lst)


  let rec check : lambda -> bool
  = fun lambda ->  
  	match lambda with
  		| V(a) -> matchi lambda []
  		| P(a,b) -> matchi b [a]
  		| C(a,b) -> (matchi a [])&&(matchi b [])
