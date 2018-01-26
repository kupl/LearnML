(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)


exception InputError

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  let rec exchange : exp * int -> exp
  = fun (e,v) -> match e with
  | X -> INT(v)
  | INT(x) -> INT(x)
  | ADD(e1,e2) -> ADD(exchange(e1,v),exchange(e2,v))
  | SUB(e1,e2) -> SUB(exchange(e1,v),exchange(e2,v))
  | MUL(e1,e2) -> MUL(exchange(e1,v),exchange(e2,v))
  | DIV(e1,e2) -> DIV(exchange(e1,v),exchange(e2,v))
  | SIGMA(e1,e2,e3) -> SIGMA(exchange(e1,v),exchange(e2,v),e3)

  let rec calculator : exp -> int
  = fun exp -> match exp with
  | INT(x) -> x
  | ADD(a,b) -> calculator(a)+calculator(b)
  | SUB(a,b) -> calculator(a)-calculator(b)
  | MUL(a,b) -> calculator(a)*calculator(b)
  | DIV(a,b) -> calculator(a)/calculator(b)
  | SIGMA(a,b,c) -> if calculator(a)=(calculator(b)+1) then 0 else (calculator(SIGMA(INT(calculator(a)+1),b,c))+calculator(exchange(c,calculator(a))))
  | X -> raise InputError 
						