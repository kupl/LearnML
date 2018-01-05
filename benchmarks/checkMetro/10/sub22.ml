(* Exercise 7 *)
type metro =
	STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec checkId (metro, env) =
	match metro with
		STATION id ->
			(List.exists (fun x -> (x = id)) env)
		| AREA (id, submetro) ->
			(checkId (submetro, (id :: env)))
		| CONNECT (submetro1, submetro2) ->
			((checkId (submetro1, env)) && (checkId (submetro2, env)))

let rec checkMetro metro =
	checkId (metro, [])
