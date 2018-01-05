(* 2015-11380 박찬양 HW2_4 *)

type metro = STATION of name
						| AREA of name * metro
						| CONNECT of metro * metro
	and name = string

let checkMetro: metro -> bool = fun mtr ->
	let rec checkIn mtrr arealist =
	 match mtrr with
	 | AREA (a,b) -> checkIn b (a::arealist)
	 | STATION a -> (List.mem a arealist)
	 | CONNECT (a,b) -> ((checkIn a arealist) && (checkIn b arealist))
	in checkIn mtr []

	