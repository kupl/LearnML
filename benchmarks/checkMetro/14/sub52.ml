type metro = 
	|STATION of name
	|AREA of name * metro
	|CONNECT of metro * metro
and name = string

let rec checking met env =
	match met with
	| STATION st -> List.mem st env
	| AREA(ar,m) ->	checking m (ar::env)
	| CONNECT(m1,m2) ->	(checking m1 env) && (checking m2 env)

let rec checkMetro met =
	(checking met [])
	

	