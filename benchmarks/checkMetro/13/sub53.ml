type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

let rec addi i list =
	match list with
	| [] -> [i]
	| h :: t -> h :: addi i t

let rec getpair (stations, metroin) =
	match metroin with
	| STATION x -> addi [x] stations
	| AREA (x, y) -> (List.map (addi x) (getpair (stations, y)))
	| CONNECT (x, y) -> List.append (getpair (stations, x)) (getpair (stations, y))

let exceptFirst list =
	match list with
	| [] -> []
	| h::t -> t

let rec checkMetro metroin =
	let checkStationName singleList = List.mem (List.hd singleList) (exceptFirst singleList) in
	List.for_all checkStationName (getpair ([], metroin))