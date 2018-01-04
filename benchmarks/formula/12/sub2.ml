type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr;;

let rec eval f =

	let rec solexpr expr =
		match expr with
		| NUM i -> i
		| PLUS (e1,e2) -> (solexpr e1)+(solexpr e2)
		| MINUS (e1, e2) -> (solexpr e1)-(solexpr e2)
	in
  	match f with
  	| TRUE -> true
  	| FALSE -> false
  	| NOT a-> not(eval a)
  	| ANDALSO (a,b)-> (eval a)&&(eval b)
  	| ORELSE (a,b) -> (eval a)||(eval b)
  	| IMPLY (a,b) -> if (eval a)=false then true
  			 else (eval a)&&(eval b)
  	| LESS (e1,e2)-> if (solexpr e1) < (solexpr e2) then true
			else false;;