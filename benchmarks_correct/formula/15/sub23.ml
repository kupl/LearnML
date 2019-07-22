
type formula = TRUE
		|FALSE
		|NOT of formula
		|ANDALSO of formula * formula
		|ORELSE of formula * formula
		|IMPLY of formula * formula
		|LESS of expr * expr
and expr = NUM of int
		|PLUS of expr * expr
		|MINUS of expr * expr


let rec eval f =  
	match f with
	|TRUE -> true
	|FALSE -> false
	|NOT f1 -> not (eval f1)
	|ANDALSO (f1, f2) -> (eval f1)&&(eval f2)
	|ORELSE (f1, f2) -> (eval f1)||(eval f2)
	|IMPLY (f1, f2) -> (match (eval f1, eval f2) 
						with (true, false) -> false |(_,_)->true)
	|LESS (e1, e2) -> (let rec evExpr e = 
								match e with
								|NUM i1 -> i1
								|PLUS (e1, e2) -> (evExpr e1) + (evExpr e2)
								|MINUS (e1, e2) -> (evExpr e1) - (evExpr e2)
						in if (evExpr e1) < (evExpr e2) then true else false)

(* TESTCASE
let f = TRUE
let f1 = NOT f
let f2 = ANDALSO (f, f1)
let f3 = ORELSE (f2, f1)
let f4 = IMPLY (f, f2)
let e = NUM 5
let e1 = NUM 9
let e2 = NUM (-3)
let e3 = MINUS (e1, e2)
let e4 = PLUS (e, e3)
let e5 = PLUS (MINUS (e4, e1), e2)
let f5 = LESS (e, e5)

let s_of_b a = if a = true then "True" else "False"

let _ = print_endline (s_of_b (eval f4))
let _ = print_endline (s_of_b (eval f5))
*)
