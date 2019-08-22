type lambda  = V of var
			| P of var * lambda
			| C of lambda * lambda
			and var = string
let rec check : lambda -> bool = 
	let rec matchStr : var list * var -> bool =
		fun(varlist,var) ->
			match varlist with
			| h::t -> if(h=var) then true
					  else matchStr(t,var)
			| [] -> false
	in
	let rec checkDeeper : lambda * var list -> bool = 
		fun(lambda,varlist) ->
			match lambda with
			|V var -> matchStr(varlist,var)
			|P (n,m) -> checkDeeper(m,[n]@varlist)
			|C (m1,m2)-> checkDeeper(m1,varlist)&&checkDeeper(m2,varlist)
	in
	fun lambda -> checkDeeper(lambda,[])
