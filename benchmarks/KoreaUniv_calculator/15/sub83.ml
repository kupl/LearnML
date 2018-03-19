type exp = X
    | INT of int
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp

let rec calculator : exp -> int
=fun e -> match e with 
    X->raise (Failure "Unvalid expression")
    |INT (x) -> x
    |ADD (x, y) -> calculator(x) + calculator(y)
    |SUB (x, y) -> calculator(x) - calculator(y)
    |MUL (x, y) -> calculator(x) * calculator(y)
    |DIV (x, y) -> calculator(x) / calculator(y)
    |SIGMA (x,y,z) -> exp_to_func(e, x)

and exp_to_func : (exp * exp) -> int
=fun(a, b) -> match a with 
    |X-> calculator (b)
    |INT (x) -> x
    |ADD (x, y) -> exp_to_func(x, b) + exp_to_func(y, b)
    |SUB (x, y) -> exp_to_func(x, b) - exp_to_func(y, b)
    |MUL (x, y) -> exp_to_func(x, b) * exp_to_func(y, b)
    |DIV (x, y) -> exp_to_func(x, b) / exp_to_func(y, b)
    |SIGMA (x,y,z) -> if(calculator(x) <= calculator(y)) then exp_to_func (z, x) + calculator( SIGMA( INT(calculator (ADD(x, INT 1))), y, z) ) else 0



