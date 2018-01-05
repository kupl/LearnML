exception FreeVariable

type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

let rec insult (e, i) =
  match e with
  | X -> (galculator i)
  | INT a -> float_of_int a
  | REAL a -> a
  | ADD (a1, a2) -> (insult (a1, i)) +. (insult (a2, i))
  | SUB (a1, a2) -> (insult (a1, i)) -. (insult (a2, i))
  | MUL (a1, a2) -> (insult (a1, i)) *. (insult (a2, i))
  | DIV (a1, a2) -> (insult (a1, i)) /. (insult (a2, i))
  | SIGMA (a, b, e) -> (sigma (int_of_float (insult (a, i)),
                               int_of_float (insult (b, i)),
                               e))
  | INTEGRAL (a, b, e) -> (gubun ((insult (a, i)), (insult (b, i)), e))
  
and sigma (a, b, e) =
  if a > b then 0.
  else ((insult (e, (INT b))) +. (sigma (a, b-1, e)))

and gubun (a, b, e) =
  if (a < b) && ((b -. a) < 0.1) then 0.
  else if (a < b) && ((b -. a) >= 0.1) then (0.1 *. (insult (e, REAL a)) +. (gubun (a +. 0.1, b, e)))
  else 0. -. (gubun (b, a, e))

and galculator e =
  match e with
  | X -> raise FreeVariable
  | INT a -> float_of_int a
  | REAL a -> a
  | ADD (a1, a2) -> (galculator a1) +. (galculator a2)
  | SUB (a1, a2) -> (galculator a1) -. (galculator a2)
  | MUL (a1, a2) -> (galculator a1) *. (galculator a2)
  | DIV (a1, a2) -> (galculator a1) /. (galculator a2)
  | SIGMA (a, b, e) -> (sigma (int_of_float (galculator a),
                               int_of_float (galculator b),
                               e))
  | INTEGRAL (a, b, e) -> (gubun ((galculator a), (galculator b), e))
