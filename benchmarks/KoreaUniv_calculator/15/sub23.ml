type exp = X | INT of int | ADD of exp * exp | SUB of exp * exp | MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp 
let rec cal p x =
	match p with
	INT a -> a
	| X -> x
	| ADD(a,b) -> cal a x + cal b x
	| SUB(a,b) -> cal a x - cal b x
	| MUL(a,b) -> cal a x * cal b x
	| DIV(a,b) -> cal a x / cal b x
	| SIGMA(a,b,c) -> if a = b then (cal c (cal a 0))
	else cal (ADD(INT (cal c (cal a 0)), SIGMA(INT ((cal a 0) + 1),b,c))) 0

let calculator p =
	cal p 0