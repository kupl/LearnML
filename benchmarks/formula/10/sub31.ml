type formula  = TRUE	
	| FALSE
	| NOT of formula
  	| ANDALSO of formula * formula
  	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec eval : formula -> bool = 
	let rec cal e = 
		match e with NUM a -> a
			|PLUS (e1,e2) -> (cal e1) + (cal e2)
			|MINUS (e1,e2) -> (cal e1) - (cal e2) in
	fun f -> match f with
		TRUE -> true
		|FALSE -> false
		|NOT a -> not (eval a)
		|ANDALSO (f1,f2) -> (eval f1) && (eval f2)
		|ORELSE (f1,f2) -> (eval f1) || (eval f2)
		|IMPLY (f1,f2) -> if ((eval f1) = true) && ((eval f2 = false)) then false
				  else true
		|LESS (e1,e2) -> (cal e1) < (cal e2)
