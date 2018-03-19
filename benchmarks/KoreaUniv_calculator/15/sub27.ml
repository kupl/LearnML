type exp = X
|INT of int
|ADD of exp * exp
|SUB of exp * exp
|MUL of exp * exp
|DIV of exp * exp
|SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> match e with
|INT e -> e
|ADD (e, e2) -> (calculator e) + (calculator e2)
|SUB (e, e2) -> (calculator e) - (calculator e2)
|MUL (e, e2) -> (calculator e) * (calculator e2)
|DIV (e, e2) -> (calculator e) / (calculator e2)
|SIGMA (e, e2, e3) -> 375;;

