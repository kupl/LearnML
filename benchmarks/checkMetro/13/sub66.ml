type metro = STATION of name
	   | AREA of name * metro
	   | CONNECT of metro * metro
and name = string
let rec deletecity(name, lis) =
	if name = List.hd(lis) then if List.length(lis)=1 then [] else  deletecity(name, List.tl(lis))
	else if List.length(lis)=1 then lis else List.hd(lis)::deletecity(name,List.tl(lis))
let rec findcity met =
	match met with
	| STATION name -> name::[]
	| AREA (name, metro) -> deletecity(name, findcity(metro))
	| CONNECT (met1, met2) -> List.rev_append (findcity(met1)) (findcity(met2))

let deletestation(name, metro) =
	let citylist = findcity(metro) in
	if deletecity(name, citylist)=[]
	then true else false

let rec checkMetro met =
	match met with
	| STATION name -> false
	| AREA (name, metro) -> if checkMetro(metro) then true 
		else deletestation(name, metro)
	| CONNECT (met1, met2) -> checkMetro(met1) && checkMetro(met2)
