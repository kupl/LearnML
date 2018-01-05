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

let rec subs: float -> exp -> float = fun i e ->
  match e with
  | X -> i
  | INT a -> float_of_int a
  | REAL a -> a
  | ADD (a, b) -> (subs i a) +. (subs i b)
  | SUB (a, b) -> (subs i a) -. (subs i b)
  | MUL (a, b) -> (subs i a) *. (subs i b)
  | DIV (a, b) -> (subs i a) /. (subs i b)
  | SIGMA (a, b, c) -> sigma (int_of_float (subs i a)) (int_of_float (subs i b)) c
  | INTEGRAL (a, b, c) -> integral (subs i a) (subs i b) c

and integral: float -> float -> exp -> float = fun a b e ->
  if (a > b) then (-1.) *. (integral b a e)
  else if (b -. a < 0.1) then 0.
  else (0.1 *. (subs a e) +. integral (a +. 0.1) b e)

and sigma: int -> int -> exp -> float = fun a b e ->
  if (a > b) then 0.
  else if (a = b) then (subs (float_of_int a) e)
  else (subs (float_of_int a) e) +. (sigma (a + 1) b e)

and galculator: exp -> float = fun e ->
  match e with
  | X -> raise FreeVariable
  | INT a -> float_of_int a
  | REAL a -> a
  | ADD (a, b) -> (galculator a) +. (galculator b)
  | SUB (a, b) -> (galculator a) -. (galculator b)
  | MUL (a, b) -> (galculator a) *. (galculator b)
  | DIV (a, b) -> (galculator a) /. (galculator b)
  | SIGMA (a, b, c) -> sigma (int_of_float (galculator a)) (int_of_float (galculator b)) c
  | INTEGRAL (a, b, c) -> integral (galculator a) (galculator b) c
