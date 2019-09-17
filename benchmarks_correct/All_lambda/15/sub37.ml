type lambda = 
	  V of var
	| P of var * lambda
	| C of lambda * lambda

and var = string

let rec isIncluded (lst, elem) = 
	match lst with
	| head::tail -> 
		if head = elem then true
		else isIncluded (tail, elem)
	| _ -> false
		
		
let rec checkPartial (areaNameLst, mtr) =
	match mtr with
	| V stnName -> isIncluded (areaNameLst, stnName)
	| C (m1, m2) ->
		if checkPartial (areaNameLst, m1) = false then false
		else checkPartial (areaNameLst, m2)
	| P (subName, subMtr) ->
		checkPartial (subName::areaNameLst, subMtr)

let check mtr =
	match mtr with
	| V _ -> false
	| C (m1, m2) -> 
		if checkPartial([], m1) = false then false
		else checkPartial([], m2)
	| P (n, m) -> checkPartial ([], P(n, m))
