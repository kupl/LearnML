type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkMetro : metro -> bool = 
	fun met -> 
	let rec stationname : metro -> name list =
		fun f -> 
			match f with
				|AREA (a, b) -> (stationname b)  
				|STATION a -> [a]
				|CONNECT (a, b) -> (List.append (stationname a) (stationname b)) in
	let rec areaname : metro -> name list =
		fun f ->
			match f with
				|AREA (a, b) -> (List.append [a] (areaname b))
				|CONNECT (a, b) -> (List.append (areaname a) (areaname b))
				|STATION a -> [] in
	let rec haveit : name list -> name list -> bool =
		fun a b ->
			let predicate : name -> bool = fun f -> (List.mem f b) in
			(List.for_all predicate a) in
	match met with
		|AREA (a, b) -> (haveit (stationname b) (areaname met))
		|CONNECT (a, b) -> (checkMetro a) && (checkMetro b)
		|STATION a -> false


