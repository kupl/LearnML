(*Problem 5*)
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp


let rec calc : exp -> int -> int
= fun e n -> match e with
|X -> n
|INT n -> n
|ADD(a, b) -> (calc a n) + (calc b n)
|SUB(a, b) -> (calc a n) - (calc b n)
|MUL(a, b) -> (calc a n) * (calc b n)
|DIV(a, b) -> (calc a n) / (calc b n)
|SIGMA(a, b, c) -> raise (Failure "Double SIGMA")

let rec calculator : exp -> int
= fun e -> match e with
|X -> raise (Failure "Wrong input")
|INT n -> n
|ADD(a,b) -> calculator a + calculator b
|SUB(a,b) -> calculator a - calculator b
|MUL(a,b) -> calculator a * calculator b
|DIV(a,b) -> calculator a / calculator b
|SIGMA(a, b, c) -> if((calculator a) > (calculator b)) then 0
else calc c (calculator a) + calculator(SIGMA(INT((calculator a)+1),b,c))
