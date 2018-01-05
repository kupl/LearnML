type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec check(metro, list) =
	match metro with
    | STATION (name) -> 
			  List.mem name list
		| CONNECT(metro1,metro2) ->
			(check(metro1, list) && check(metro2, list))
		| AREA(name, metro) ->
			(check(metro, List.append [name] list));;
			
let checkMetro (metro) =
		check(metro, []);;

