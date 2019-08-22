

type lambda = V of var
		|	P of var * lambda
		|	C of lambda * lambda
and var = string;;

let check mat =
	let rec checkStringInList(li, st) =
		match li with
		|	[] -> false
		|	a :: remain -> if (st= a) then true
						else checkStringInList(remain, st)
	in
	let rec checkStationInArea(listOfArea, subMat) =	
		match subMat with
		|	V s -> checkStringInList(listOfArea, s)
		|	C (m1, m2) -> (checkStationInArea(listOfArea, m1)
								&& checkStationInArea(listOfArea, m2))
		|	P(a, m) -> let newlist = a :: listOfArea in
						checkStationInArea(newlist, m)
	in
	match mat with
	|	V s -> false
	|	C (m1, m2) -> false
	|	P (a, m) -> checkStationInArea([a], m)
	;;

(* exercise test
Printf.printf( "suposed to be true\n" );
check( P( "a", V "a" ) );;
check( P( "a", P("a", V "a") ) );;
check( P( "a", P("b", C(V "a", V "b"))) );;
check( P( "a", C(V "a", P("b", V "a"))) );;
Printf.printf( "suposed to be false\n" );
check( P( "a", V "b" ) );;
check( P( "a", C(V "a", P("b", V "c"))) );;
check( P( "a", P("b", C(V "a", V "c"))) );;
exercise *)
