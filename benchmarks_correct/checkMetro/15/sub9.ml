type lambda = 	V of var
		| P of var * lambda
		| C of lambda * lambda
and var = string

let rec station_list x =
	match x with
	| V n -> [n]
	| P (n, m) -> (station_list m)
	| C (m1, m2) -> List.append (station_list m1) (station_list m2)

let rec area_list x = 
	match x with
	| V n -> []
	| P (n, m) -> List.append [n] (area_list m)
	| C (m1, m2) -> List.append (area_list m1) (area_list m2)

let rec is_included (x, y) =
	match x with
	| [] -> true
	| hd::tl -> (List.mem hd y) && is_included (tl, y)

(* check if stations from lambda x is in area list l *)
let rec checkList (x, l) =
	match x with
	| V n -> List.mem n l
	| P (n, m) -> checkList (m, n::l)
	| C (m1, m2) -> (checkList (m1, l)) && (checkList (m2, l))

let rec check x =
	checkList (x, [])
(*
	match x with
	| V n -> false
	| P (n, m) -> (match m with
			 | V n_sub -> List.mem n_sub (area_list x)
			 | P (n1, m1) -> is_included ((station_list m), (area_list x)) && (check m)
			 | C (m1, m2) -> (match (m1, m2) with
						| (P (m1_n, m1_m), _) -> is_included ((station_list m), (area_list x)) && (check
						| (_, P (m2_n, m2_m)) -> 
						| (_, _) -> )

let rec check (x: lambda): bool =
	match x with
	| V n -> false
	| P (n, m) -> (match m with
			 | V n_sub -> List.mem n (station_list m)
			 | P (n1, m1) -> List.mem n (station_list m) || (check m)
			 | C (m1, m2) -> (match (m1, m2) with
						| (P (m1_n, m1_m), _) -> List.mem n (station_list m) || (check m1)
						| (_, P (m2_n, m2_m)) -> List.mem n (station_list m) || (check m2)
						| (_, _) -> List.mem n (station_list m)
						)
			)
(* List.mem n (station_list m)) *)
	| C (m1, m2) -> (*(match (m1, m2) with
				| (P (m_sub, n_sub), _) -> (check (P (m_sub, n_sub)))
				| (_, P (m_sub2, n_sub2)) -> (check (P (m_sub2, n_sub2)))
				| (_, _) -> false)
*)
(check m1) && (check m2)
*)
