type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkMetro met = 
	check(met, [])

and check (met,l) = 
	match met with
        |STATION n -> if(idSearch(n, l)) then true else false
        |AREA(n, metr) -> check(metr, [n]@l)
        |CONNECT(metr1, metr2) -> check(metr1, l) && check(metr2, l)

and idSearch(id,l) =
	match l with
	|[] -> false
	|head::tail -> 	if(head = id) then true
		else idSearch(id,tail)
