type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec in_list el lst =
	match lst with [] -> false
	| head::tail -> (head = el) || in_list el tail

let rec checkMetro_sub metro lst =
        match metro with
	STATION s_name -> in_list s_name lst
	| AREA (a_name, metro_sub) -> checkMetro_sub metro_sub (a_name::lst)
       	| CONNECT (a,b) -> checkMetro_sub a lst && checkMetro_sub b lst

let checkMetro metro =
	checkMetro_sub metro []

