exception DividedByZero of string
type expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr | MULT of expr * expr | DIVIDE of expr * expr | MAX of expr list
let rec eval exp =
	let getMax stringList  =
		let rec subGetMax subString firstNum =
			match subString with 
			h::t -> ( if (h < firstNum ) then (subGetMax t firstNum)
				  else (subGetMax t h))
			|[] -> firstNum
		in
	match stringList with
	[] -> 0
	|h::t -> (subGetMax t h)
	in
match exp with
NUM a -> a
|PLUS (a,b) -> (eval a) + (eval b)
|MINUS (a,b) -> (eval a) - (eval b)
|MULT (a,b) -> (eval a) * (eval b)
|DIVIDE (a,b) -> (if ((eval b) == 0) then (raise (DividedByZero "invalid computation"))
		  else (eval a) / (eval b))
|MAX exprList -> getMax (List.map eval exprList)
	
