type metro = STATION of name|AREA of name*metro|CONNECT of metro*metro and name=string

let addToList lst newInlst=newInlst::lst
let rec check prevAreaName metro=
	match metro with
	|STATION(stationName)-> List.mem stationName prevAreaName
	|AREA(areaName, nextMetro)-> check (areaName::prevAreaName) nextMetro
	|CONNECT(metroA, metroB)-> check prevAreaName metroA && check prevAreaName metroB

let rec checkMetro inpt =
	match inpt with
	| STATION(name)-> true
	| AREA(name, metro)-> check (name::[]) metro
	| CONNECT(metroA, metroB)-> checkMetro metroA && checkMetro metroB
