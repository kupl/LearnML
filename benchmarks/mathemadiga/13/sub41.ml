type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

exception FreeVariable

let rec galculator exp =
  let rec galaux e =
    match e with
    | (X, x) -> x
    | (INT i, x) -> INT i
    | (REAL f, x) -> REAL f
    | (ADD (e1, e2), x) -> ADD (galaux (e1, x), galaux (e2, x))
    | (SUB (e1, e2), x) -> SUB (galaux (e1, x), galaux (e2, x))
    | (MUL (e1, e2), x) -> MUL (galaux (e1, x), galaux (e2, x))
    | (DIV (e1, e2), x) -> DIV (galaux (e1, x), galaux (e2, x))
    | (SIGMA (e1, e2, e3), x) -> SIGMA (galaux (e1, x), galaux (e2, x), galaux (e3, e1))
    | (INTEGRAL (e1, e2, e3), x) -> INTEGRAL (galaux (e1, x), galaux (e2, x), galaux (e3, e1)) in

  match exp with
  | X -> raise FreeVariable
  | INT i -> float(i)
  | REAL f -> f
  | ADD (e1, e2) -> (galculator e1) +. (galculator e2)
  | SUB (e1, e2) -> (galculator e1) -. (galculator e2)
  | MUL (e1, e2) -> (galculator e1) *. (galculator e2)
  | DIV (e1, e2) -> (galculator e1) /. (galculator e2)
  | SIGMA (e1, e2, e3) ->
      if ((int_of_float (galculator e1)) > (int_of_float (galculator e2))) then 0.
      else (galculator (galaux (e3, e1))) +. (galculator (SIGMA (ADD (e1, INT 1), e2, e3)))
  | INTEGRAL (e1, e2, e3) ->
      if ((galculator e1) > (galculator e2)) then galculator (INTEGRAL (e2, e1, MUL (INT (-1), e3)))
      else if (((galculator e2) -. (galculator e1)) < 0.1) then 0.
      else ((galculator (galaux (e3, e1))) *. 0.1) +. (galculator (INTEGRAL (ADD (e1, REAL 0.1), e2, e3)))
