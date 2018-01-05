(* 2012-11230 Kim sangmin *)
type metro = STATION of name
		   | AREA of name*metro
		   | CONNECT of metro*metro
and name = string

let rec checkMetro : metro -> bool = fun x ->
	let rec helper : metro*string list -> bool = fun (met, env) ->
		match met with
		| STATION(n) -> if(List.mem n env) then true else false
		| AREA(n, m) -> if(List.mem n env) then helper(m,env) else helper(m, n::env)
		| CONNECT(m1, m2) -> if(helper(m1,env) && helper(m2,env)) then true else false
	in
	helper(x, [])


