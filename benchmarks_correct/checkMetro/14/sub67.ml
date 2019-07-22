type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec check(metro, lst) =
	match metro with
    | STATION (name) -> 
			  List.mem name lst
		| CONNECT(metro1,metro2) ->
			(check(metro1, lst) && check(metro2, lst))
		| AREA(name, metro) ->
			(check(metro, List.append [name] lst));;
			
let checkMetro (metro) =
		check(metro, []);;

