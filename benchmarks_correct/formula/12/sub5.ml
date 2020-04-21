 type formula = True
| False
| Not of formula
| AndAlso of formula * formula| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

 let rec eval boo =
	let rec oper x =
  		match x with
  			|Num p -> p
  			|Plus(x1,x2) -> (oper x1) + (oper x2)
  			|Minus(x1,x2) -> (oper x1) - (oper x2)
  	in
  match boo with
  		| True -> true
  		| False -> false
  		| Not j -> not (eval j)
  		| AndAlso (j1,j2) -> (eval j1) && (eval j2)
  		| OrElse (j1,j2) -> (eval j1) || (eval j2)
  		| Imply (j1,j2) -> (not (eval j1)) || (eval j2)
  		| Equal (n1,n2) -> if (oper n1) = (oper n2) then true
					else false

