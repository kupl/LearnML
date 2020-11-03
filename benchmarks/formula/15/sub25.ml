(* 2011-10915 / 생명과학부/ 신지민/ Homework 1-4 *)

type formula = True
              | False
              | Not of formula
              | AndAlso of formula * formula
              | OrElse of formula * formula
              | Imply of formula * formula
              | Equal of exp * exp
    and  exp = Num of int
              | Plus of exp * exp
              | Minus of exp * exp


let rec cal : exp -> int = fun e ->
	match e with
	|Num i -> i
	|Plus(e1,e2) -> begin
		let i1 = cal e1 in
		let i2 = cal e2 in
		i1+i2
			end
	|Minus(e1,e2) -> begin
		let i1 = cal e1 in
		let i2 = cal e2 in
		i1-i2
			end


let rec eval : formula -> bool = fun f ->
	match f with
	|True -> true
	|False -> false
	|Not a -> if(eval a==true) then false
		  else true
	|AndAlso(a,b) -> if(eval a==true && eval b==true) then true	
			else false
	|OrElse(a,b) -> if(eval a==true || eval b==true) then true
			else false
	|Imply(a,b) -> if(eval a==true && eval b==false) then false
			else true
	|Equal(a,b) -> if(cal a = cal b) then true
			else false

(*
let b = eval True
let _= begin
	if(b==true) then  print_endline("true\n") 
	else print_endline("false\n")
	end

let b = eval (Not(AndAlso(True,True)))
let _= begin
	if(b==true) then  print_endline("true\n") 
	else print_endline("false\n")
	end

let b = eval (Imply((Equal(Plus(Num 1,Num 4), Minus(Num 1000, Num 10)),AndAlso(False,False))))
let _= begin
	if(b==true) then  print_endline("true\n") 
	else print_endline("false\n")
	end
*)
