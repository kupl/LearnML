(* 2008-11874 Lee, Sujee *)
(* EXERCISE 7 *)

type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkMetro metro = (* checkMetro : metro -> bool = <fun> *)
	let rec takeArea(metro,idlist) = (* metro : metro / idlist : string list*)
		match (metro,idlist) with
			| (STATION id, idlist) -> List.mem id idlist
			| (AREA(id,met),idlist) -> takeArea(met,(List.append [id] idlist))
			| (CONNECT(met1,met2),idlist) -> (takeArea(met1,idlist)) && (takeArea(met2,idlist))
		in
	match metro with
		| STATION id -> false (* if metro contains only STATION, then false. *)
		| AREA(id,met) -> takeArea(met,[id])
		| CONNECT(met1,met2) -> (checkMetro(met1)) && (checkMetro(met2))

