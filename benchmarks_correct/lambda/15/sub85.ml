type lambda = V of var
					| P of var*lambda
					| C of lambda*lambda
and var=string

let check lambda = 
	let rec is_in_area s a =
		match a with
		| [] -> false
		| h::t -> if h=s then true else is_in_area s t
	in

	let rec aux env = function
		| V s -> if (is_in_area s env) then true else false
		| P( a, m ) -> aux (a::env) m
		| C ( m1, m2 ) -> ( aux env m1 ) && ( aux env m2 )
	in

	aux [] lambda


(*
let print_bool b = print_string (string_of_bool b) ; print_string "\n"


let r1 = P("a", V "a")
let r2 = P("a", P("a", V "a") )
let r3 = P("a", P("b", C( V "a", V "b") ) )
let r4 = P("a", C( V "a", P("b", V "a")))
let f1 = P("a", V "b")
let f2 = P("a", C( V "a", P("b", V "c") ) )
let f3 = P("a", P("b", C( V "a", V "c")))
let testcase = r1::r2::r3::r4::f1::f2::f3::[];;
List.iter print_bool ( List.map check testcase )

let a1 = P("a", V "a")
let a2 = P("a", P("a", V "a"))
let a3 = P("a", P("b", C(V "a", V "b")))
let a4 = P("a", C(V "a", P("b", V "a")))
let a5 = P("a", V "b")
let a6 = P("a", C( V "a", P("b", V "c")))
let a7 = P("a", P("b", C(V "a", V "c")))
let testcase2 = a1::a2::a3::a4::a5::a6::a7::[];;
List.iter print_bool ( List.map check testcase2 )
*)


