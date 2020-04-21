(* CSE/ 2004-11920 / Yeseong Kim/ Prob 5*)

type formula =
		True
	|	False
	|	Not of formula
	|	AndAlso of formula * formula
	|	OrElse of formula * formula
	|	Imply of formula * formula
	|	Equal of exp * exp and exp = Num of int
	|	Plus of exp * exp
	|	Minus of exp * exp

let rec eval f = 
	let rec subEval num = 
		match num with
			Num(v) -> v
		|	Plus(e1, e2) -> (subEval(e1) + subEval(e2))
		|	Minus(e1, e2) -> (subEval(e1) - subEval(e2))
	in
	match f with
			True -> true
		|	False -> false
		|	Not(ff) -> (not(eval(ff)))
		|	AndAlso(f1, f2) -> ((eval(f1)) && (eval(f2)))
		|	OrElse(f1, f2) -> ((eval(f1)) || (eval(f2)))
		|	Imply(f1, f2) -> ((not(eval(f1))) || (eval(f1) && eval(f2)))
		|	Equal(e1, e2) -> (subEval(e1) = subEval(e2))
