type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let checkMetro metro =
	let rec checkMetro_sub metro env =
		match metro with
		| STATION name -> 
			(List.exists (fun x -> x = name) env)
		| AREA (name, m) ->
			(checkMetro_sub m (name::env))
		| CONNECT (m1, m2) ->
			(checkMetro_sub m1 env) && (checkMetro_sub m2 env)
	in
	(checkMetro_sub metro [])
