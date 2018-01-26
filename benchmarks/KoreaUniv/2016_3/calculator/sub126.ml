(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  let rec checkx : (exp*exp) -> exp
  = fun (a,b) -> match a with
    |X -> b
    |INT(x) -> INT(x)
    |ADD(x,y) -> ADD(checkx(x,b), checkx(y,b))
    |SUB(x,y) -> SUB(checkx(x,b), checkx(y,b))
    |MUL(x,y) -> MUL(checkx(x,b), checkx(y,b))
    |DIV(x,y) -> DIV(checkx(x,b), checkx(y,b))
    |SIGMA(x,y,z) -> SIGMA(checkx(x,b),checkx(y,b),z)

  let rec calculator : exp -> int
  = fun exp -> match exp with
		|X -> raise NotImplemented
		|INT(a) ->  a 
		|ADD(a,b) -> calculator(a)+calculator(b)
		|SUB(a,b) -> calculator(a)-calculator(b)
		|MUL(a,b) -> calculator(a)*calculator(b)
		|DIV(a,b) -> calculator(a)/calculator(b)
		|SIGMA(first, last, yap) 
		-> if (calculator(first)=calculator(last)) 
				then (calculator(checkx(yap,first)))  else (calculator(SIGMA(INT(calculator(first)+1),last,yap))+calculator(checkx(yap,first))) 
