
exception FreeVariable
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec cal : exp -> int
=fun exp ->
  match exp with
  | X -> raise FreeVariable
  | INT e1 -> e1
  | ADD(e1, e2) -> cal e1 + cal e2
  | SUB(e1, e2) -> cal e1 - cal e2
  | MUL(e1, e2) -> cal e1 * cal e2
  | DIV(e1, e2) -> cal e1 / cal e2
  | SIGMA(p, q, f) ->
	if (cal p)>(cal q) then 0 else eval(f,p) + cal(SIGMA(ADD(p,INT 1),q,f))
  and eval (f,x) = 
	 match f with
  | X -> cal x
  | INT z -> z
  | ADD(f1,f2) -> eval (f1,x) + eval (f2,x)
  | SUB(f1,f2) -> eval (f1,x) - eval (f2,x)
  | MUL(f1,f2) -> eval (f1,x) * eval (f2,x)
  | DIV(f1,f2) -> eval (f1,x) / eval (f2,x)
  | SIGMA (i,j,f1) -> cal (SIGMA (INT (eval (i,x)), INT (eval (j,x)), f1))