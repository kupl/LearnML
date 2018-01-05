type metro = 
	  STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro

and name = string

let rec isIncluded (lst, elem) = 
	match lst with
	| head::tail -> 
		if head = elem then true
		else isIncluded (tail, elem)
	| _ -> false
		
		
let rec checkPartial (areaNameLst, mtr) =
	match mtr with
	| STATION stnName -> isIncluded (areaNameLst, stnName)
	| CONNECT (m1, m2) ->
		if checkPartial (areaNameLst, m1) = false then false
		else checkPartial (areaNameLst, m2)
	| AREA (subName, subMtr) ->
		checkPartial (subName::areaNameLst, subMtr)

let checkMetro mtr =
	match mtr with
	| STATION _ -> false
	| CONNECT (m1, m2) -> 
		if checkPartial([], m1) = false then false
		else checkPartial([], m2)
	| AREA (n, m) -> checkPartial ([], AREA(n, m))
