(* CSE/ 2004-11920 / Yeseong Kim/ Prob 5*)

type formula =
		TRUE
	|	FALSE
	|	NOT of formula
	|	ANDALSO of formula * formula
	|	ORELSE of formula * formula
	|	IMPLY of formula * formula
	|	LESS of expr * expr and expr = NUM of int
	|	PLUS of expr * expr
	|	MINUS of expr * expr

let rec eval f = 
	let rec subEval num = 
		match num with
			NUM(v) -> v
		|	PLUS(e1, e2) -> (subEval(e1) + subEval(e2))
		|	MINUS(e1, e2) -> (subEval(e1) - subEval(e2))
	in
	match f with
			TRUE -> true
		|	FALSE -> false
		|	NOT(ff) -> (not(eval(ff)))
		|	ANDALSO(f1, f2) -> ((eval(f1)) && (eval(f2)))
		|	ORELSE(f1, f2) -> ((eval(f1)) || (eval(f2)))
		|	IMPLY(f1, f2) -> ((not(eval(f1))) || (eval(f1) && eval(f2)))
		|	LESS(e1, e2) -> (subEval(e1) < subEval(e2))
