type metro = STATION of name
		 | AREA of name * metro
		 | CONNECT of metro * metro
and name = string

let rec isinlst mtr lst =
	match (mtr,lst) with
	|(_, []) -> false
	|(_, _) -> if mtr=(List.hd lst) then true else isinlst mtr (List.tl lst)


let rec checkMetro1 mtr lst=
	match mtr with 
		|STATION st -> isinlst st lst
		|AREA (name, mt)-> checkMetro1 mt (name::lst)
		|CONNECT (m1, m2)-> checkMetro1 m1 lst && checkMetro1 m2 lst

let rec checkMetro mtr = checkMetro1 mtr []

(*
let t1 = checkMetro(AREA("a", STATION "a"))
 let t2=checkMetro(AREA("a", AREA("a", STATION "a")))
 let t3=checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))
 let t4=checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))
 let t5=checkMetro(AREA("a", STATION "b"))
 let t6=checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))
 let t7=checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))

*)
