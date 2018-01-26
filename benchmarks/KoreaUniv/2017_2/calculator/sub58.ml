(* problem 5 *)
type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
let rec form 
= fun exp n -> match exp with 
  | X -> n
  | INT(x) -> x
  | ADD(x, y) -> (form x n) + (form y n)
  | SUB(x, y) -> (form x n) - (form y n)
  | MUL(x, y) -> (form x n) * (form y n)
  | DIV(x, y) -> (form x n) / (form y n)
let rec result
= fun exp n1 n2 -> match n1 with 
  | _ -> if (n1 = n2) then (form exp n1) else ((form exp n1) + (result exp (n1+1) n2))
let rec scope_x
= fun exp -> match exp with
  | INT(x) -> x
  | ADD(x, y) -> (scope_x x) + (scope_x y)
  | SUB(x, y) -> (scope_x x) - (scope_x y)
  | MUL(x, y) -> (scope_x x) * (scope_x y)
  | DIV(x, y) -> (scope_x x) / (scope_x y)
  | SIGMA(x, y, z) -> result z (scope_x x) (scope_x y) 
let rec scope_y
= fun exp -> match exp with
  | INT(y) -> y
  | ADD(x, y) -> (scope_y x) + (scope_y y)
  | SUB(x, y) -> (scope_y x) - (scope_y y)
  | MUL(x, y) -> (scope_y x) * (scope_y y)
  | DIV(x, y) -> (scope_y x) / (scope_y y)
  | SIGMA(x, y, z) -> result z (scope_y x) (scope_y y) 
let rec expression
= fun exp -> match exp with
  | SIGMA(_, _, z) -> z
let rec check
= fun exp -> match exp with
  | SIGMA(_, _, _) -> 1
  | _ -> 0
let rec calculator : exp -> int
= fun e -> match e with
  | INT(x) -> x
  | ADD(x, y) -> (calculator x) + (calculator y)
  | SUB(x, y) -> (calculator x) - (calculator y)
  | MUL(x, y) -> (calculator x) * (calculator y)
  | DIV(x, y) -> (calculator x) / (calculator y)
  | SIGMA(x, y, z) -> if ((check (expression e)) = 0) then result (expression e) (scope_x x) (scope_y y) else ((scope_y y) - (scope_x x) + 1)*(calculator z)
