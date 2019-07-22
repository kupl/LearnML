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

let rec eval formulain= 
	let rec calc exprin= 
		match exprin with
			NUM(x) -> x
			| PLUS(x,y) -> (calc(x))+(calc(y))
			| MINUS(x,y) -> (calc(x))-(calc(y))
	in
	match formulain with
		TRUE -> true
		| FALSE -> false
		| NOT(interform) -> not (eval(interform))
		| ANDALSO(interform1,interform2) -> eval(interform1) && eval(interform2)
		| ORELSE(interform1,interform2) -> eval(interform1) || eval(interform2)
		| IMPLY(interform1,interform2) -> not (eval(interform1) && (not(eval(interform2))))
		| LESS(expr1,expr2) -> (calc(expr1)) < (calc(expr2))