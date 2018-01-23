(* C:\Users\owner\Desktop\Homework 1(7).ml *)

type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string ;;

let rec checkMetro metr =

	let makeBool a b = (a=b)
	
  let rec checkArea(namArr, metr) =
		match metr with
		STATION na -> 	if (List.length namArr) == 1 then makeBool (List.hd namArr) na
				else (makeBool (List.hd namArr) na) || checkArea((List.tl namArr), metr)
		| CONNECT(met1, met2) -> checkArea(namArr, met1) && checkArea(namArr, met2)
		| AREA(nam1, met1) -> checkArea(nam1::namArr, met1) in

	match metr with
	AREA(nam, met) -> checkArea([nam], met)
	| CONNECT(met1, met2) -> checkMetro(met1) && checkMetro(met2)
	| _ -> false ;;

