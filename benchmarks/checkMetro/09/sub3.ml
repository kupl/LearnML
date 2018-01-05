type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro and name = string;;

let rec checkmetromap a =
	let rec checkhelper lst m = match m with
	    | STATION name -> List.exists (fun x -> if x = name then true else false) lst
	    | CONNECT(x, y) -> (checkhelper lst x) && (checkhelper lst y)
	    | AREA(name, c) -> (checkhelper (name::lst) c)
	in
	match a with
		| STATION name    -> false
		| AREA(name, c) -> (checkhelper (name::[]) c)
		| CONNECT(x, y) -> (checkmetromap x) && (checkmetromap y)
;;