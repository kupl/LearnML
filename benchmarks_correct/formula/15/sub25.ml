(* 2011-10915 / 생명과학부/ 신지민/ Homework 1-4 *)

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


let rec cal : expr -> int = fun e ->
	match e with
	|NUM i -> i
	|PLUS(e1,e2) -> begin
		let i1 = cal e1 in
		let i2 = cal e2 in
		i1+i2
			end
	|MINUS(e1,e2) -> begin
		let i1 = cal e1 in
		let i2 = cal e2 in
		i1-i2
			end


let rec eval : formula -> bool = fun f ->
	match f with
	|TRUE -> true
	|FALSE -> false
	|NOT a -> if(eval a==true) then false
		  else true
	|ANDALSO(a,b) -> if(eval a==true && eval b==true) then true	
			else false
	|ORELSE(a,b) -> if(eval a==true || eval b==true) then true
			else false
	|IMPLY(a,b) -> if(eval a==true && eval b==false) then false
			else true
	|LESS(a,b) -> if(cal a < cal b) then true
			else false

(*
let b = eval TRUE
let _= begin
	if(b==true) then  print_endline("true\n") 
	else print_endline("false\n")
	end

let b = eval (NOT(ANDALSO(TRUE,TRUE)))
let _= begin
	if(b==true) then  print_endline("true\n") 
	else print_endline("false\n")
	end

let b = eval (IMPLY((LESS(PLUS(NUM 1,NUM 4), MINUS(NUM 1000, NUM 10)),ANDALSO(FALSE,FALSE))))
let _= begin
	if(b==true) then  print_endline("true\n") 
	else print_endline("false\n")
	end
*)
