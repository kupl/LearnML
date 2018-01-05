exception FreeVariable

type exp = X | INT of int | REAL of float | ADD of exp * exp | SUB of exp * exp | MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp | INTEGRAL of exp * exp * exp

let rec sigma s e exp =
  if (s > e) then 0.
  else ((num_galculator true (float_of_int s) exp) +. (sigma (s + 1) e exp))

and integral s e exp = 
  if (e-.s < 0.1) then 0.
  else ((num_galculator true s exp) *. 0.1  +. (integral (s +. 0.1) e exp))

and num_galculator allow_X num exp =
  match exp with
  | X ->
	if(allow_X) then num
	else (raise FreeVariable)
  | INT a -> (float_of_int a)
  | REAL a -> a
  | ADD (a, b) -> (num_galculator allow_X num a) +. (num_galculator allow_X num b)
  | SUB (a, b) -> (num_galculator allow_X num a) -. (num_galculator allow_X num b)
  | MUL (a, b) -> (num_galculator allow_X num a) *. (num_galculator allow_X num b)
  | DIV (a, b) -> (num_galculator allow_X num a) /. (num_galculator allow_X num b)
  | SIGMA (a, b, e) -> (sigma (int_of_float (num_galculator allow_X num a)) (int_of_float (num_galculator allow_X num b)) e)
  | INTEGRAL (a, b, e) -> 
    if a < b then (integral (num_galculator allow_X num a) (num_galculator allow_X num b) e)
	else (-1. *. (integral (num_galculator allow_X num b) (num_galculator allow_X num a) e))

let galculator exp = (num_galculator false 0. exp)
