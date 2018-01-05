(* HW 1-5 / 2007-11603 / 컴퓨터공학부 / 이영준 *)

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


let rec eval f =
	(* 두 expr를 계산해주는 함수*)
	let rec cal e =
		match e with
		NUM x -> x
		| PLUS (x, y) -> (cal x) + (cal y)
		| MINUS (x, y) -> (cal x) - (cal y)
	in

	match f with
	TRUE -> true
	| FALSE -> false
	| NOT f -> if (eval f) then false else true
	| ANDALSO (f1, f2) -> (eval f1)&&(eval f2)
	| ORELSE (f1, f2) -> (eval f1)||(eval f2)
	| IMPLY (f1, f2) -> if (eval f1) = true && (eval f2) = false then
				false 
			    else true	
        | LESS (e1, e2) -> (cal e1) < (cal e2)

