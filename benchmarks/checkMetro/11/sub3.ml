(*2006-11681 °­Çö¼®*)
exception Invalid_input of string

type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro
and name = string

let checkMetro m =
	
	let rec check_sub(m ,l) =
		let rec checkArea(s, l) =
			match l with
			[] -> false
			| h::t -> (if h=s then true
					   else checkArea(s, t))
		in

		match m with
		STATION s -> checkArea(s, l)
		| AREA(a,m) -> check_sub(m,a::l)
		| CONNECT(m0,m1) -> check_sub(m0,l) && check_sub(m1,l)
	in

	match m with
	STATION s -> raise (Invalid_input "STATION only")
	| _ -> check_sub(m,[])
(*
let m1 = AREA("a", STATION "b")
let m2 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
let m3 = AREA("a", AREA("b",CONNECT(STATION "a", STATION "c")))
let m4 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
let m5 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
*)

	
					
