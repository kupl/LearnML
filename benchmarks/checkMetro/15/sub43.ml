type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

(*
let rec arealist metrol =
	match metrol with
	| STATION i -> []
	| AREA (i1, i2) -> (if List.mem i1 metrol then arealist i2 
				else i1 :: arealist i2)
	| CONNECT (i1, i2) -> arealist i1 :: arealist i2
*)

(*
let changeMetro list1 =
	let _ list2 : (list1 * list)
*)
(*
let rec makearea : metro -> 'a list = function list1 ->
	match list1 with
	| AREA (i1, i2) -> i1 :: makearea i2
	| STATION i -> []
	| CONNECT (i1, i2) -> makearea i1 @ makearea i2
*)

let rec checkMetrof (metrol, list1)  =
	match metrol with
	| STATION i -> (if List.mem i list1 then true else false)
	| CONNECT (i1, i2) -> checkMetrof (i1,list1) && checkMetrof (i2,list1)
	| AREA (i3, i4) -> checkMetrof (i4, i3::list1)

let checkMetro metrol = checkMetrof (metrol, [])
