(* 생명과학부 / 2011-10915 / 신지민 / Homework 1-2 *)

type formula = TRUE 
                      | FALSE
                      | NOT of formula
                      | ANDALSO of formula * formula
                      | ORELSE of formula * formula
                      | IMPLY of formula * formula
                      | LESS of expr * expr
         and  expr = NUM of int
                   | PLUS of expr * expr
                   | MINUS of expr * expr

let rec inverter : expr -> int = fun expr ->
	match expr with
		|NUM i -> i
		|PLUS (e1,e2)  -> begin 
			 	 let i1 = inverter e1 in
				 let i2 = inverter e2 in
				 i1 + i2
				  end
		|MINUS (e1,e2) -> begin
				 let i1 = inverter e1 in
				 let i2 = inverter e2 in
				 i1 - i2
				  end
		
let rec eval : formula -> bool = fun formula ->
	match formula with
	  	| TRUE -> true
		| FALSE -> false
		| NOT f -> if (eval f==true) then false else true
		| ANDALSO (f1,f2) -> if(eval f1==true && eval f2==true) then true else false
		| ORELSE (f1,f2) -> if(eval f1==true || eval f2==true) then true else false
		| IMPLY (f1,f2) -> if(eval f1==true && eval f2==false) then false else true
		| LESS (e1,e2) -> begin
				let i1 = inverter e1 in
				let i2 = inverter e2 in
				if i1<i2 then true else false
				  end

(*
let e1 = PLUS(PLUS(NUM 100, NUM 11), NUM 7)
let e2 = MINUS (PLUS(NUM 101, MINUS(NUM 30, NUM 10)), NUM 2)
let q2 = eval(ANDALSO (FALSE,FALSE))
let q3 = eval(NOT (NOT TRUE))
let q4 = eval(IMPLY (ANDALSO(FALSE,TRUE),FALSE))
let q5 = eval(IMPLY (IMPLY(TRUE,TRUE),FALSE))
let q6 = eval(LESS(e1, e2))
let _=
	if q2==true then print_endline("TRUE") else  print_endline("FALSE")
let _=
	if q3==true then print_endline("TRUE") else  print_endline("FALSE")
let _=
	if q4==true then print_endline("TRUE") else  print_endline("FALSE")
let _=
	if q5==true then print_endline("TRUE") else  print_endline("FALSE")
let _=
	if q6==true then print_endline("TRUE") else  print_endline("FALSE")
*)
