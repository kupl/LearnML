(*Computer Science Engineering 2015-12683 Kim Jaein*)
type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro
and name = string

let rec checkhelp (input, a_list) = 
	match input with
	|STATION a -> List.exists (fun x -> x = a) a_list
	|AREA (a, inside) -> checkhelp (inside, (List.append [a;] a_list))
	|CONNECT (a, b) -> checkhelp (a, a_list) && checkhelp (b, a_list)

let checkMetro (input:metro) =
	checkhelp (input, [])

