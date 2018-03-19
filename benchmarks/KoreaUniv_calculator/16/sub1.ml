
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let calculator : exp -> int
  = fun exp -> let rec value : exp->exp->int = fun e1 e2 ->( match e1 with
	| X -> value e2 e2
	| INT i -> i
	| ADD(x,y) -> value x e2 + value y e2
	| SUB(x,y) -> value x e2 - value y e2
	| MUL(x,y) -> value x e2 * value y e2
	| DIV(x,y) -> value x e2 / value y e2
	| SIGMA(x,y,z) ->let rec si : exp -> int ->int -> int = fun n x y ->
	if x<y then value n  (INT x) + si n (x+1) y 
	else value n (INT x) in si z (value x e2) (value y e2)) in value exp (INT 0);;  (* TODO *)
