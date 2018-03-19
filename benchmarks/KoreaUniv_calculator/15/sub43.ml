type exp = X
| INT of int
| ADD of exp * exp
|	SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let toexp : int -> exp
= fun x -> INT x

let rec toint : (int * exp) -> int
= fun(n, e) -> match (n, e) with
(0, X) -> raise(Failure"Error")
|(_, X) -> n
|(_, INT a) -> a
|(_, ADD (a, b)) -> (toint (n, a)) + (toint (n, b))
|(_, SUB (a, b)) -> (toint (n, a)) - (toint (n, b))
|(_, MUL (a, b)) -> (toint (n, a)) * (toint (n, b))
|(_, DIV (a, b)) -> (toint (n, a)) / (toint (n, b))
|(_, SIGMA(a, b, m)) -> if toint(0, a) < toint(0, b)
	then toint(toint(0, a), m) + toint(0, SIGMA(toexp(toint(0, a) +1), b, m))
	else toint(toint(0, b), m)

let calculator : exp -> int
= fun e -> match e with
X -> raise(Failure"Error")
|INT a -> a
|ADD (n, m) -> (toint (0, n)) + (toint (0, m))
|SUB (n, m) -> (toint (0, n)) - (toint (0, m))
|MUL (n, m) -> (toint (0, n)) * (toint (0, m))
|DIV (n, m) -> (toint (0, n)) / (toint (0, m))
|SIGMA (a, b, n) -> toint(0, SIGMA(a, b, n))
