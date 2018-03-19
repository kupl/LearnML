type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
 
let rec replace_X : exp * int -> exp
= fun (e, x) ->
match e with 
X -> INT x
|INT n -> INT n
|SIGMA (a, b, c) -> raise (Failure "why here? error")
|ADD(a, b) -> ADD((replace_X(a, x)), (replace_X(b, x)))
|SUB(a, b) -> SUB((replace_X(a, x)), (replace_X(b, x)))
|MUL(a, b) -> MUL((replace_X(a, x)), (replace_X(b, x)))
|DIV(a, b) -> DIV((replace_X(a, x)), (replace_X(b, x)))

let rec num_calculate : exp -> int
= fun e ->
match e with
X -> raise (Failure "error")
|INT n -> n
|ADD (a, b) -> (num_calculate a) + (num_calculate b)
|SUB (a, b) -> (num_calculate a) - (num_calculate b)
|MUL (a, b) -> (num_calculate a) * (num_calculate b)
|DIV (a, b) -> (num_calculate a) / (num_calculate b)
|SIGMA (a, b, c) -> let start = (num_calculate a) in
		let endsig = (num_calculate b) in
			if start < (endsig + 1) then
				let func = replace_X (c, start) in
 				(num_calculate func) + num_calculate(SIGMA(ADD(INT start, INT 1), INT endsig, c))
			else 0

let calculator : exp -> int
= fun e ->
match e with 
X -> raise (Failure "error")
|INT n -> n
|_ -> num_calculate e
