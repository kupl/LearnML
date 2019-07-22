 type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

 let rec eval boo =
	let rec oper x =
  		match x with
  			|NUM p -> p
  			|PLUS(x1,x2) -> (oper x1) + (oper x2)
  			|MINUS(x1,x2) -> (oper x1) - (oper x2)
  	in
  match boo with
  		| TRUE -> true
  		| FALSE -> false
  		| NOT j -> not (eval j)
  		| ANDALSO (j1,j2) -> (eval j1) && (eval j2)
  		| ORELSE (j1,j2) -> (eval j1) || (eval j2)
  		| IMPLY (j1,j2) -> (not (eval j1)) || (eval j2)
  		| LESS (n1,n2) -> if (oper n1) < (oper n2) then true
					else false

