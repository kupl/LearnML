type expr = NUM of int | PLUS of expr*expr | MINUS of expr*expr | MULT of expr*expr | DIVIDE of expr*expr | MAX of expr list

let rec eval expr00 =
        match expr00 with
        | NUM(int00) -> int00
        | PLUS(int01, int02) -> ((eval(int01)) + (eval(int02)))
        | MINUS(int01, int02) -> ((eval(int01)) - (eval(int02)))
        | MULT(int01, int02) -> ((eval(int01)) * (eval(int02)))
        | DIVIDE(int01, int02) -> ((eval(int01)) / (eval(int02)))
        | MAX (exprlist) ->
		match exprlist with
		| [] -> 0
		| expr01::[] -> (eval expr01)
		| expr01::exprlist' ->
			begin
			if ((eval expr01) >= (eval (MAX exprlist'))) then (eval expr01)
			else (eval (MAX exprlist'))
			end  
		
