exception INVALID_INPUT
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let inc : exp -> exp
= fun e -> match e with
INT x -> INT (x+1)
|_ -> raise INVALID_INPUT

let rec str_replace : exp -> exp -> exp
= fun subject replace -> match subject with
X -> replace
| INT n -> INT n
| ADD (e1, e2) -> ADD (str_replace e1 replace, str_replace e2 replace)
| SUB (e1, e2) -> SUB ((str_replace e1 replace), (str_replace e2 replace))
| MUL (e1, e2) -> MUL ((str_replace e1 replace), (str_replace e2 replace))
| DIV (e1, e2) -> DIV ((str_replace e1 replace), (str_replace e2 replace))
| SIGMA (s, e, f) -> SIGMA ((str_replace s replace), (str_replace e replace), (str_replace f replace))

(*let rec calculator : exp -> int*)
let rec calculator : exp -> int
=fun e -> match e with
|X -> raise INVALID_INPUT
|INT n -> n
|ADD (e1, e2) -> calculator e1 + calculator e2
|SUB (e1, e2) -> calculator e1 - calculator e2
|MUL (e1, e2) -> calculator e1 * calculator e2
|DIV (e1, e2) -> calculator e1 / calculator e2
|SIGMA (s, e, f) -> if (s = e) then calculator (str_replace f s) else calculator (str_replace f s) + calculator (SIGMA (inc s, e, f))
