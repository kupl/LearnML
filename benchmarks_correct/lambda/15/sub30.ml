(* C:\Users\saigoy\Desktop\check.ml *)

type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string;;

let check : lambda -> bool = fun lambda ->
  	let rec isProperStation (var, areaList) = 
  	match areaList with
  	| [] -> false
  	| hd::tl -> 
  	(
  		if(hd= var) then true
  		else (isProperStation (var, tl))
  	)	in
  
  	let rec check_Aux (lambda, areaList) = 
  	match lambda with
  	| V n -> isProperStation(n, areaList)
  	| P (n, m) -> check_Aux (m, n::areaList)
  	| C (lm, rm) -> ( check_Aux(lm, areaList) )&& ( check_Aux(rm, areaList) )	in
  check_Aux(lambda, []);;

