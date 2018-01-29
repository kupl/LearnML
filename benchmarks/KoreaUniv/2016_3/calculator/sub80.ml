
type exp =
| X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp ->
let rec calc exp num fl output = (
	match exp with
	X-> if fl = 0 then raise(Failure "match failure") else num
	| INT x->x
	| ADD(x, y)-> (calc x num fl output) + (calc y num fl output)
	| SUB(x, y)-> (calc x num fl output) - (calc y num fl output)
	| MUL(x, y)-> (calc x num fl output)*(calc y num fl output)
	| DIV(x, y)-> if (calc y num fl output) = 0 then raise(Failure "denominator is zero") else (calc x num fl output) / (calc y num fl output)
	| SIGMA(x, y, z)->
	let num = calc x num fl output in
	let fl = 1 in
	(if (calc x num fl output)>(calc y num fl output) then output
	else let output = output + (calc z num fl output) in
		calc(SIGMA(INT((calc x num fl output) + 1), INT(calc y num fl output), z)) num fl output)
	) in calc exp 0 0 0