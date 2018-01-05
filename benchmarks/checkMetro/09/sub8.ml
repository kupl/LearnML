
type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let checkMetro_in metro env =
	match metro with
	 STATION(name) -> List.mem name env
	|AREA(name,metro) -> checkMetro_in metro name::env
	|CONNECT(metro1,metro2) -> (checkMetro_in metro1 env) && (checkMetro_in metro2 env)
	
let checkMetro metro =
	checkMetro_in metro [] 
