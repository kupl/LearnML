type name = string;;

let rec print_list = function 
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l

type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro;;

let rec contains = fun (l, item)->
	match l with
	|[] -> false 
	|h :: tail ->
		if(item = h) then true
		else contains(tail, item);;

let rec metro = fun (l, m)->
	match m with
	|STATION n -> contains(l, n)
	|AREA (id, area) -> metro(id::l, area) 
	|CONNECT (m1, m2) -> metro(l,m1) && metro(l,m2);;

let checkMetro = fun m -> metro ([], m);;

