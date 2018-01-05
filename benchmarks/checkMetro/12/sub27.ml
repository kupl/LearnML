(* hw1-8 *)
(* 2010-11687 Keunjun choi *)

type metro = STATION of name
                  | AREA of name * metro
                  | CONNECT of metro * metro
and name = string
let checkMetro m =
	let rec check (ms, l) =
		match ms with
		| STATION a -> List.mem a l
		| AREA (a, b) -> check (b, a::l)
		| CONNECT (a, b) -> check (a, l) && check (b, l)
	in
	check (m, [])
