type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let rec z_fun : exp*int -> int
= fun(a,b) ->
match a with
|X -> b
|INT x -> x
|ADD(x,y) -> z_fun(x,b)+z_fun(y,b)
|SUB(x,y) -> z_fun(x,b)-z_fun(y,b)
|MUL(x,y) -> z_fun(x,b)*z_fun(y,b)
|DIV(x,y) -> z_fun(x,b)/z_fun(y,b)


let rec calculator : exp -> int
=fun e ->
match e with
|SIGMA(x,y,z) ->if z_fun(x,0)< z_fun(y,0)+1 then z_fun(z,z_fun(x,0))+calculator(SIGMA(INT(z_fun(x,0)+1),y,z)) else 0
|_ -> z_fun(e,0);;