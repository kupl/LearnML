type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string

let checkMetro _metro =
	let rec checkMetroRec (_metro, _areaList) =
		match _metro with
		STATION(_name) -> List.mem _name _areaList
		| AREA(_name, __metro) -> checkMetroRec(__metro, _name::_areaList)
		| CONNECT(_metro1, _metro2) -> checkMetroRec(_metro1, _areaList) &&  checkMetroRec(_metro2, _areaList)
	in

	checkMetroRec(_metro, [])

