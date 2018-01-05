type metro = STATION of name
	   | AREA of name * metro
	   | CONNECT of metro * metro

and name = string


let add_element x set =
	x::set ;;

let rec check x set =
	if (List.mem x set) then true else false;;



let rec sub_checkMetro metro set=
	match metro with
	AREA (x, m) -> (sub_checkMetro m (add_element x set))
	|STATION n -> (check n set)
	|CONNECT (m1, m2) -> (sub_checkMetro m1 set) && (sub_checkMetro m2 set) ;;
	

let rec checkMetro metro =
	sub_checkMetro metro [];
