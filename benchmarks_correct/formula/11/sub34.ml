(* 2009-13384, CHO Hyunik *)



type formula =	TRUE | FALSE 
		| NOT of formula 
		| ANDALSO of formula * formula 
		| ORELSE of formula * formula
		| IMPLY of formula * formula
		| LESS of expr * expr
and expr =	NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr



let rec eval formula1 =

(* LOCAL FUNCTION exprCalculation : function that translate formula to integer *)
	let rec exprCalculation expr1 =
		match expr1 with
		NUM(a) -> a
		| PLUS(a, b) -> (exprCalculation a)+(exprCalculation b)
		| MINUS(a, b) -> (exprCalculation a)-(exprCalculation b) in


	match formula1 with
	TRUE -> true
	| FALSE -> false
	| NOT(a) -> not(eval a)
	| ANDALSO(a, b) -> (eval a)&&(eval b)
	| ORELSE(a, b) -> (eval a)||(eval b)
	| IMPLY(a, b) -> (eval b)||(not(eval a))
	| LESS(a, b) -> (exprCalculation a) < (exprCalculation b)