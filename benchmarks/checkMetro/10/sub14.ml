exception Error of string
type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
	and name = string

let rec env_checker : metro -> string list -> bool =
	(function mymet ->
		(function myls ->
			(match mymet with
				STATION sta -> if (List.mem sta myls) then true else false
				| AREA (thename, themet) -> if (List.mem thename myls) then (env_checker themet myls) 
								else (env_checker themet (thename :: myls))
				| CONNECT (bm, cm) -> ((env_checker bm myls) && (env_checker cm myls)))))

let checkMetro alp =
	try
		(env_checker alp [])
	with e -> raise (Error "You are idiot.")	
