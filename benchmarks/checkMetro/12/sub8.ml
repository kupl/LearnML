

type metro = STATION of name
		|	AREA of name * metro
		|	CONNECT of metro * metro
and name = string;;

let checkMetro mat =
	let rec checkStringInList(li, st) =
		match li with
		|	[] -> false
		|	a :: remain -> if (compare st a) == 0 then true
						else checkStringInList(remain, st)
	in
	let rec checkStationInArea(listOfArea, subMat) =	
		match subMat with
		|	STATION s -> checkStringInList(listOfArea, s)
		|	CONNECT (m1, m2) -> (checkStationInArea(listOfArea, m1)
								&& checkStationInArea(listOfArea, m2))
		|	AREA(a, m) -> let newlist = a :: listOfArea in
						checkStationInArea(newlist, m)
	in
	match mat with
	|	STATION s -> false
	|	CONNECT (m1, m2) -> false
	|	AREA (a, m) -> checkStationInArea([a], m)
	;;

(* exercise test
Printf.printf( "suposed to be true\n" );
checkMetro( AREA( "a", STATION "a" ) );;
checkMetro( AREA( "a", AREA("a", STATION "a") ) );;
checkMetro( AREA( "a", AREA("b", CONNECT(STATION "a", STATION "b"))) );;
checkMetro( AREA( "a", CONNECT(STATION "a", AREA("b", STATION "a"))) );;
Printf.printf( "suposed to be false\n" );
checkMetro( AREA( "a", STATION "b" ) );;
checkMetro( AREA( "a", CONNECT(STATION "a", AREA("b", STATION "c"))) );;
checkMetro( AREA( "a", AREA("b", CONNECT(STATION "a", STATION "c"))) );;
exercise *)
