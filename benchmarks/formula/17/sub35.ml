type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec eval : formula -> bool = fun f ->
	let rec cal_expr : expr -> int = fun e ->
		match e with
		| NUM i -> i
		| PLUS (e1,e2) -> cal_expr(e1) + cal_expr(e2)
		| MINUS (e1,e2) -> cal_expr(e1) - cal_expr(e2)
	in

	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT s -> not (eval (s))
	| ANDALSO (f1,f2) -> (eval (f1) && eval (f2))
	| ORELSE (f1,f2) -> (eval (f1) || eval (f2))
	| IMPLY (f1,f2) -> (not (eval (f1)) || eval(f2))
	| LESS (e1,e2) -> 
		if(cal_expr (e1) < cal_expr (e2)) then true
		else false

(*
let a61 = eval TRUE 
let a62 = eval FALSE 
let a63 = eval (NOT TRUE) 
let a64 = eval (ANDALSO (TRUE, FALSE)) 
let a65 = eval (ORELSE (TRUE, FALSE)) 
let a66 = eval (LESS (PLUS(NUM 3, NUM 4), MINUS(NUM 7, NUM 8)))
*)
