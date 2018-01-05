open Pervasives

type name = string

type metro = STATION of name
						| AREA of name * metro
						| CONNECT of metro * metro

let rec checkList station metList =
	match metList with
	| [] -> false
	| h :: t -> if (compare station h) == 0 then true 
		else (checkList station t)

let rec printstringlist listed =
	match listed with
	| [] -> print_string ("")
	| h :: t -> print_string (h); (printstringlist t)

let checkMetro met =
	let rec makeMetroList met metList =
		match met with
		| AREA (a, b) -> makeMetroList b (a :: metList)
		| STATION (a) -> 
			if (checkList a metList) == true then true
			else false
		| CONNECT (a, b) ->
			if ((makeMetroList a metList)&&(makeMetroList b metList)) == true
				then true
			else false in
	makeMetroList met []

