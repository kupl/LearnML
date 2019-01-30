type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> match exp with
  | X -> raise (Failure "X is not initialized")
  | INT n -> n
  | ADD (x1, x2) -> calculator x1 + calculator x2
  | SUB (x1, x2) -> calculator x1 - calculator x2
  | MUL (x1, x2) -> calculator x1 * calculator x2
  | DIV (x1, x2) -> let denom = calculator x2 in if denom = 0 then raise (Failure "Division by ZERO") else calculator x1 / denom
  | SIGMA (x1, x2, x) -> let start = calculator x1 and goal = calculator x2 in
    if start = goal then formula(x, x1) else calculator(ADD(INT(formula(x, x1)), INT(calculator(SIGMA(INT(start+1), x2, x)))))
    
and formula : exp * exp -> int
= fun (exp, k) -> match exp with
  | X -> calculator k
  | INT n -> n
  | ADD (x1, x2) -> formula(x1, k) + formula(x2, k)
  | SUB (x1, x2) -> formula(x1, k) - formula(x2, k)
  | MUL (x1, x2) -> formula(x1, k) * formula(x2, k)
  | DIV (x1, x2) -> let denom = formula(x2, k) in if denom = 0 then raise (Failure "Division by ZERO") else formula(x1, k) / denom
  | SIGMA (start, goal, exp) -> calculator (SIGMA (start, goal, exp));;