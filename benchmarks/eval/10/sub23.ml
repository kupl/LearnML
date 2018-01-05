(* Exercise 3 *)
exception DividedByZero

type expr =
	NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list

let rec max_exprlist exprlist nowval =
	match exprlist with
		[] ->
			nowval
		| h :: t ->
			(let hval = (eval h) in
			if (hval < nowval) then
				(max_exprlist t nowval)
			else
				(max_exprlist t hval))
and
eval expr =
	match expr with
		NUM a ->
			a
		| PLUS (a, b) ->
			((eval a) + (eval b))
		| MINUS (a, b) ->
			((eval a) - (eval b))
		| MULT (a, b) ->
			((eval a) * (eval b))
		| DIVIDE (a, b) ->
			let evalb = (eval b) in
			if (evalb = 0) then
				raise DividedByZero
			else
				((eval a) / evalb)
		| MAX [] ->
			0
		| MAX exprlist ->
			(max_exprlist exprlist min_int)
