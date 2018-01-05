(* 생명과학부 / 2011-10915 / 신지민 / Homework 2-3 *)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
  and name = string

let rec checkwithList = fun(mylist,m) ->
	match m with
	|STATION(n) -> if(List.mem n mylist) then true else false
	|AREA(n_in,m_in) -> begin
			let mylist = n_in :: mylist in
			checkwithList(mylist,m_in)
			    end
	|CONNECT(m1, m2) -> if(checkwithList(mylist,m1)&&checkwithList(mylist,m2)) then true else false
	

let checkMetro: metro -> bool = fun m ->
	checkwithList([],m)

(*
let a = AREA("a", STATION "a")
let b = AREA("a", AREA("a", STATION "a"))
let c = AREA("a", AREA("b", CONNECT(STATION "a",STATION "b")))
let d = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
let e = AREA("a", STATION "b")
let f = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
let g = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))

let a1 = checkMetro a
let b1 = checkMetro b
let c1 = checkMetro c
let d1 = checkMetro d
let e1 = checkMetro e
let f1 = checkMetro f
let g1 = checkMetro g

let _= print_endline(string_of_bool a1)
let _= print_endline(string_of_bool b1)
let _= print_endline(string_of_bool c1)
let _= print_endline(string_of_bool d1)
let _= print_endline(string_of_bool e1)
let _= print_endline(string_of_bool f1)
let _= print_endline(string_of_bool g1)
*)


