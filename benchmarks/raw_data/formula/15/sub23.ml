
type formula = True
		|False
		|Not of formula
		|AndAlso of formula * formula
		|OrElse of formula * formula
		|Imply of formula * formula
		|Equal of exp * exp
and exp = Num of int
		|Plus of exp * exp
		|Minus of exp * exp


let rec eval f =  
	match f with
	|True -> true
	|False -> false
	|Not f1 -> not (eval f1)
	|AndAlso (f1, f2) -> (eval f1)&&(eval f2)
	|OrElse (f1, f2) -> (eval f1)||(eval f2)
	|Imply (f1, f2) -> (match (eval f1, eval f2) 
						with (true, false) -> false |(_,_)->true)
	|Equal (e1, e2) -> (let rec evExpr e = 
								match e with
								|Num i1 -> i1
								|Plus (e1, e2) -> (evExpr e1) + (evExpr e2)
								|Minus (e1, e2) -> (evExpr e1) - (evExpr e2)
						in if (evExpr e1) = (evExpr e2) then true else false)

(* TESTCASE
let f = True
let f1 = Not f
let f2 = AndAlso (f, f1)
let f3 = OrElse (f2, f1)
let f4 = Imply (f, f2)
let e = Num 5
let e1 = Num 9
let e2 = Num (-3)
let e3 = Minus (e1, e2)
let e4 = Plus (e, e3)
let e5 = Plus (Minus (e4, e1), e2)
let f5 = Equal (e, e5)

let s_of_b a = if a = true then "True" else "False"

let _ = print_endline (s_of_b (eval f4))
let _ = print_endline (s_of_b (eval f5))
*)
