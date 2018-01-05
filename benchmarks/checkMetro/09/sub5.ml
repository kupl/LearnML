exception Error of string

(* EX8 : checkMetro *)
type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
			and name = string
(* definition of metro *)

let rec checkMetro a =
	(* function check : check the metro is well formed using list that has names of areas as an element *)
	let rec check mt lst =
		match mt with
			STATION name -> List.mem name lst
			(* check the name of the station is a member of list *)
			| AREA ( nm, mt ) -> check mt ( nm :: lst )
			(* add name into the list and check metro recursively *)
			| CONNECT ( m1, m2 ) -> ( check m1 lst ) && ( check m2 lst ) in
			(* check all of two metro are well formed *)
	check a []
