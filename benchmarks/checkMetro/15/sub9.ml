type metro = 	STATION of name
		| AREA of name * metro
		| CONNECT of metro * metro
and name = string

let rec station_list x =
	match x with
	| STATION n -> [n]
	| AREA (n, m) -> (station_list m)
	| CONNECT (m1, m2) -> List.append (station_list m1) (station_list m2)

let rec area_list x = 
	match x with
	| STATION n -> []
	| AREA (n, m) -> List.append [n] (area_list m)
	| CONNECT (m1, m2) -> List.append (area_list m1) (area_list m2)

let rec is_included (x, y) =
	match x with
	| [] -> true
	| hd::tl -> (List.mem hd y) && is_included (tl, y)

(* check if stations from metro x is in area list l *)
let rec checkList (x, l) =
	match x with
	| STATION n -> List.mem n l
	| AREA (n, m) -> checkList (m, n::l)
	| CONNECT (m1, m2) -> (checkList (m1, l)) && (checkList (m2, l))

let rec checkMetro x =
	checkList (x, [])
(*
	match x with
	| STATION n -> false
	| AREA (n, m) -> (match m with
			 | STATION n_sub -> List.mem n_sub (area_list x)
			 | AREA (n1, m1) -> is_included ((station_list m), (area_list x)) && (checkMetro m)
			 | CONNECT (m1, m2) -> (match (m1, m2) with
						| (AREA (m1_n, m1_m), _) -> is_included ((station_list m), (area_list x)) && (checkMetro
						| (_, AREA (m2_n, m2_m)) -> 
						| (_, _) -> )

let rec checkMetro (x: metro): bool =
	match x with
	| STATION n -> false
	| AREA (n, m) -> (match m with
			 | STATION n_sub -> List.mem n (station_list m)
			 | AREA (n1, m1) -> List.mem n (station_list m) || (checkMetro m)
			 | CONNECT (m1, m2) -> (match (m1, m2) with
						| (AREA (m1_n, m1_m), _) -> List.mem n (station_list m) || (checkMetro m1)
						| (_, AREA (m2_n, m2_m)) -> List.mem n (station_list m) || (checkMetro m2)
						| (_, _) -> List.mem n (station_list m)
						)
			)
(* List.mem n (station_list m)) *)
	| CONNECT (m1, m2) -> (*(match (m1, m2) with
				| (AREA (m_sub, n_sub), _) -> (checkMetro (AREA (m_sub, n_sub)))
				| (_, AREA (m_sub2, n_sub2)) -> (checkMetro (AREA (m_sub2, n_sub2)))
				| (_, _) -> false)
*)
(checkMetro m1) && (checkMetro m2)
*)
