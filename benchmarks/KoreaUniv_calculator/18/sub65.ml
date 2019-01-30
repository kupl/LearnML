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
  | X -> raise (Failure "X Doesn't Speicified")
  | INT n -> n
  | ADD (e1, e2) -> calculator e1 + calculator e2
  | SUB (e1, e2) -> calculator e1 - calculator e2
  | MUL (e1, e2) -> calculator e1 * calculator e2
  | DIV (e1, e2) -> calculator e1 / calculator e2
  | SIGMA (x1, x2, e) -> if calculator x1 = calculator x2 then
    let rec calx : exp -> int -> int
    = fun e1 n -> match e1 with
      | X -> n
      | INT n -> n
      | ADD (e2, e3) -> calx e2 n + calx e3 n
      | SUB (e2, e3) -> calx e2 n - calx e3 n
      | MUL (e2, e3) -> calx e2 n * calx e3 n
      | DIV (e2, e3) -> calx e2 n / calx e3 n
      | SIGMA (xc1, xc2, e2) -> calculator (SIGMA (xc1, xc2, e2))
    in calx e (calculator x1)
  else calculator (SIGMA (x1, x1, e)) + calculator (SIGMA (ADD(x1, INT 1), x2, e));;
