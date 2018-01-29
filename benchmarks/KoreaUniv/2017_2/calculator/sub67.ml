(*problem 5*)
type exp = X
|INT of int
|ADD of exp * exp
|SUB of exp * exp
|MUL of exp * exp
|DIV of exp * exp
|SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> let rec defineexp (e,x) = 
match e with
|X -> x
|INT a -> a
|ADD (a, b) 
-> defineexp (a,x) + defineexp (b,x)
|SUB (a, b) -> defineexp (a,x) - defineexp (b,x)
|MUL (a, b) -> defineexp (a,x) * defineexp (b,x)
|DIV (a, b) -> defineexp (a,x) / defineexp (b,x)
|SIGMA (a, b, f) -> defineexp ( SIGMA (ADD(a, INT(1)),b,f),x) in defineexp (e,1)