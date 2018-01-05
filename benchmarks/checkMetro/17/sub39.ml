type metro = 	STATION of name
		| AREA of name * metro
		| CONNECT of metro * metro
and name = string

let rec checkMetro m =
	let rec checkMetroProxy n areaList =
		match n with
		| STATION x -> List.mem x areaList
		| AREA (x, y) -> checkMetroProxy y (x :: areaList)
		| CONNECT (x, y) -> (checkMetroProxy x areaList) && (checkMetroProxy y areaList)
	in
	checkMetroProxy m []

