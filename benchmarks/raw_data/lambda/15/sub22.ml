(* 생명과학부 / 2011-10915 / 신지민 / Homework 2-3 *)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string

let rec checkwithList = fun(mylist,m) ->
	match m with
	|V(n) -> if(List.mem n mylist) then true else false
	|P(n_in,m_in) -> begin
			let mylist = n_in :: mylist in
			checkwithList(mylist,m_in)
			    end
	|C(m1, m2) -> if(checkwithList(mylist,m1)&&checkwithList(mylist,m2)) then true else false
	

let check: lambda -> bool = fun m ->
	checkwithList([],m)

(*
let a = P("a", V "a")
let b = P("a", P("a", V "a"))
let c = P("a", P("b", C(V "a",V "b")))
let d = P("a", C(V "a", P("b", V "a")))
let e = P("a", V "b")
let f = P("a", C(V "a", P("b", V "c")))
let g = P("a", P("b", C(V "a", V "c")))

let a1 = check a
let b1 = check b
let c1 = check c
let d1 = check d
let e1 = check e
let f1 = check f
let g1 = check g

let _= print_endline(string_of_bool a1)
let _= print_endline(string_of_bool b1)
let _= print_endline(string_of_bool c1)
let _= print_endline(string_of_bool d1)
let _= print_endline(string_of_bool e1)
let _= print_endline(string_of_bool f1)
let _= print_endline(string_of_bool g1)
*)


