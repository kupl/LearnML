exception DividedByZero

type expr = NUM of int
		  | PLUS of expr * expr
		  | MINUS of expr * expr
		  | MULT of expr * expr
		  | DIVIDE of expr * expr
		  | MAX of expr list

let rec eval e = match e with
  NUM a -> a
| PLUS(a, b) ->
	(eval a) + (eval b)
| MINUS(a, b) ->
	(eval a) - (eval b)
| MULT(a, b) ->
	(eval a) * (eval b)
| DIVIDE(a, b) ->
	if (eval b) = 0 then raise DividedByZero else (eval a) / (eval b)
| MAX lst ->
	let rec maximum lst = match lst with
	  [] -> 0
	| v::l ->
		let v1 = eval v in
		let v2 = maximum l in
		if v1 >= v2 then v1 else v2
	in
	maximum lst