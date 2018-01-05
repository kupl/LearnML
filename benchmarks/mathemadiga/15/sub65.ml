exception FreeVariable

type exp = 
  | X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp
  
let rec galSigma: int -> int -> exp -> float = fun x1 x2 e ->
  if x1 > x2 then 0.
  else (galX e (float_of_int x1)) +. (galSigma (x1 + 1) x2 e)

and galIntegral: float -> float -> exp -> float = fun x1 x2 e ->
  if (x1 > x2) then (galIntegral x2 x1 (SUB (REAL 0., e)))
  else if x1 +. 0.1 > x2 then 0.
  else ((galX e x1) *. 0.1) +. (galIntegral (x1 +. 0.1) x2 e)

and galX: exp -> float -> float = fun e x ->
  match e with
  | X -> x
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> (galX e1 x) +. (galX e2 x)
  | SUB (e1, e2) -> (galX e1 x) -. (galX e2 x)
  | MUL (e1, e2) -> (galX e1 x) *. (galX e2 x)
  | DIV (e1, e2) -> (galX e1 x) /. (galX e2 x)
  | SIGMA (e1, e2, e3) -> 
    (galSigma (int_of_float (galX e1 x)) (int_of_float (galX e2 x)) e3)
  | INTEGRAL (e1, e2, e3) ->
    (galIntegral (galX e1 x) (galX e2 x) e3)

let rec galculator: exp -> float = fun e ->
  match e with
  | X -> raise FreeVariable
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> (galculator e1) +. (galculator e2)
  | SUB (e1, e2) -> (galculator e1) -. (galculator e2)
  | MUL (e1, e2) -> (galculator e1) *. (galculator e2)
  | DIV (e1, e2) -> (galculator e1) /. (galculator e2)
  | SIGMA (e1, e2, e3) -> 
    (galSigma (int_of_float (galculator e1)) (int_of_float (galculator e2)) e3)
  | INTEGRAL (e1, e2, e3) ->
    (galIntegral (galculator e1) (galculator e2) e3)
