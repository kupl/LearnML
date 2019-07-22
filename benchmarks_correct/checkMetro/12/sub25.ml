type metro =
	STATION		of name
	|AREA		of name * metro
	|CONNECT	of metro * metro
and name = string

let rec checkMetro met =
	
	let rec checkMetroTmp areaList metTemp =
		match metTemp with
		STATION a	-> List.mem a areaList
		|AREA (a, b)	-> checkMetroTmp (a::areaList) b
		|CONNECT (a, b)	-> (checkMetroTmp areaList a) && (checkMetroTmp areaList b)
	in

	match met with
	STATION a	-> false
	|AREA (a, b)	-> checkMetroTmp [a] b
	|CONNECT (a, b)	-> (checkMetro a) && (checkMetro b)
