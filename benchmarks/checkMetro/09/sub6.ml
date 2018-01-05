type metro = STATION of name
			|AREA of name * metro
			|CONNECT of metro * metro
and name = string

let rec checkMetro a =
	let rec cm l a =
		match a with
		STATION(name) -> List.mem name l
		|AREA(name, metro) -> cm (List.append l [name]) metro
		|CONNECT(metro, metro2) -> (cm l metro) && (cm l metro2) in
	cm [] a
