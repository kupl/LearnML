type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string



let rec checkMetro met = 
	match met with
	|STATION n -> false
	|AREA(n, metr) -> (idSearch(n, idList metr)) || (checkMetro metr)
	|CONNECT(metr1, metr2) -> (checkMetro metr1) && (checkMetro metr2)

and idList met =
	match met with
        |STATION n -> n::[]
        |AREA(n, metr) -> idList metr
        |CONNECT(metr1, metr2) -> (idList metr1) @ (idList metr2)

and idSearch(id,l) =
	match l with
	|[] -> false
	|head::tail -> 	if(head = id) then true
		else idSearch(id,tail)
