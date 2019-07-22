type metro = STATION of name
					| AREA of name*metro
					| CONNECT of metro*metro
and name=string

let checkMetro metro = 
	let rec is_in_area s a =
		match a with
		| [] -> false
		| h::t -> if h=s then true else is_in_area s t
	in

	let rec aux env = function
		| STATION s -> if (is_in_area s env) then true else false
		| AREA( a, m ) -> aux (a::env) m
		| CONNECT ( m1, m2 ) -> ( aux env m1 ) && ( aux env m2 )
	in

	aux [] metro


(*
let print_bool b = print_string (string_of_bool b) ; print_string "\n"


let r1 = AREA("a", STATION "a")
let r2 = AREA("a", AREA("a", STATION "a") )
let r3 = AREA("a", AREA("b", CONNECT( STATION "a", STATION "b") ) )
let r4 = AREA("a", CONNECT( STATION "a", AREA("b", STATION "a")))
let f1 = AREA("a", STATION "b")
let f2 = AREA("a", CONNECT( STATION "a", AREA("b", STATION "c") ) )
let f3 = AREA("a", AREA("b", CONNECT( STATION "a", STATION "c")))
let testcase = r1::r2::r3::r4::f1::f2::f3::[];;
List.iter print_bool ( List.map checkMetro testcase )

let a1 = AREA("a", STATION "a")
let a2 = AREA("a", AREA("a", STATION "a"))
let a3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
let a4 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
let a5 = AREA("a", STATION "b")
let a6 = AREA("a", CONNECT( STATION "a", AREA("b", STATION "c")))
let a7 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
let testcase2 = a1::a2::a3::a4::a5::a6::a7::[];;
List.iter print_bool ( List.map checkMetro testcase2 )
*)


