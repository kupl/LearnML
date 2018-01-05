(* C:\OCaml\lib\garculator.ml *)

exception FreeVariable

type exp = X
  	|INT of int
  	|REAL of float
  	|ADD of exp * exp
  	|SUB of exp * exp
  	|MUL of exp * exp
  	|DIV of exp * exp
  	|SIGMA of exp * exp * exp
  	|INTEGRAL of exp * exp * exp

let float_int_float a = (float_of_int (int_of_float a))

let rec galc_calculator (exp, number) = 
  match exp with
  |X -> number
  |INT a -> (float_of_int a)
  |REAL a -> a
  |ADD (a, b) -> galc_calculator (a, number) +. galc_calculator (b, number)
  |SUB (a, b) -> galc_calculator (a, number) -. galc_calculator (b, number)
  |MUL (a, b) -> galc_calculator (a, number) *. galc_calculator (b, number)
  |DIV (a, b) -> galc_calculator (a, number) /. galc_calculator (b, number)
  |SIGMA (a, b, func) -> 
      (if (galc_calculator (a, number)) -. (galc_calculator (b, number)) > 0.0 then 0.0
	  else (galc_calculator (func, (float_int_float (galc_calculator (a, number))))) +. 
	  (galc_calculator ((SIGMA ((REAL ((galc_calculator (a, number)) +. 1.0)), b, func)), number)))
  |INTEGRAL (a, b, func) ->
      (if (abs_float ((galc_calculator (a, number)) -. (galc_calculator (b, number)))) < 0.1  then 0.0
	  else if ((galc_calculator (a, number)) -. (galc_calculator (b, number))) > 0.0 
	  then -.(galc_calculator ((INTEGRAL (b, a, func)), number))
	  else (galc_calculator (func, (galc_calculator (a, number)))) *. 0.1 +. 
	  (galc_calculator ((INTEGRAL ((REAL ((galc_calculator (a, number)) +. 0.1)), b, func)), number)))

let rec galculator exp =
  match exp with
  |X -> raise (FreeVariable)
  |INT a -> (float_of_int a)
  |REAL a -> a
  |ADD (a, b) -> (galculator a) +. (galculator b)
  |SUB (a, b) -> (galculator a) -. (galculator b)
  |MUL (a, b) -> (galculator a) *. (galculator b)
  |DIV (a, b) -> (galculator a) /. (galculator b)
  |SIGMA (a, b, func) -> 
      (if (galculator a) -. (galculator b) > 0.0 then 0.0
	  else (galc_calculator (func, (float_int_float (galculator a)))) +. 
	  (galculator (SIGMA ((REAL ((galculator a) +. 1.0)), b, func))))
  |INTEGRAL (a, b, func) -> 
      (if (abs_float ((galculator a) -. (galculator b))) < 0.1 then 0.0
	  else if ((galculator a) -. (galculator b)) > 0.0 then -.(galculator (INTEGRAL (b, a, func)))
	  else ((galc_calculator (func, (galculator a))) *. 0.1 +. 
	  (galculator (INTEGRAL ((REAL ((galculator a) +. 0.1)), b, func)))))
