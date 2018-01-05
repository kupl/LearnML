(*hw1-7 컴퓨터 공학부 2008-11641 신희식*) 

type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

let checkMetro met =
	let rec sub_checkMetro (checklist, metr) =
		match metr with
		(STATION a) ->
			(List.mem a checklist)
		|(AREA (a,b)) ->
			(sub_checkMetro ((a::checklist), b))
		|(CONNECT (a,b)) ->
			((sub_checkMetro (checklist,a)) 
			 && (sub_checkMetro (checklist, b)))
	in
	(sub_checkMetro ([], met))
