(* file name : ex4.ml *)
(* author : Jisoon Park (jspark@ropas.snu.ac.kr) *)
(* date : 2013-09-13 *)
(* Exercise 4 *)
type metro = STATION of name
						| AREA of name * metro
						| CONNECT of metro * metro
and name = string

let rec checkMetroSub : metro * string list -> bool
 = fun (mtr, areaList) ->
	match mtr with
	| STATION st -> List.exists (fun x -> x = st) areaList
	| AREA (ar, mtr2) -> checkMetroSub(mtr2, ar::areaList)
	| CONNECT (mtr1, mtr2) -> checkMetroSub(mtr1, areaList) && checkMetroSub(mtr2, areaList) 

let checkMetro : metro -> bool
 = fun mtr ->
	checkMetroSub (mtr, [])
