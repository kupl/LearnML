type exp = X
  |INT of int
  |REAL of float
  |ADD of exp * exp
  |SUB of exp * exp
  |MUL of exp * exp
  |DIV of exp * exp
  |SIGMA of exp * exp * exp
  |INTEGRAL of exp * exp * exp
exception FreeVariable
let rec galrec (n,x,y) =
  match n with
  |X -> if x == true then y else (raise FreeVariable)
  |INT a -> float_of_int a
  |REAL a -> a
  |ADD (left, right) -> (galrec (left,x,y))+.(galrec (right,x,y))
  |SUB (left, right) -> (galrec (left,x,y))-.(galrec (right,x,y))
  |MUL (left, right) -> (galrec (left,x,y))*.(galrec (right,x,y))
  |DIV (left, right) -> (galrec (left,x,y))/.(galrec (right,x,y))
  |SIGMA (st, en, func) -> 
      let a = int_of_float (galrec (st,x,y)) in
      let b = int_of_float (galrec (en,x,y)) in
      if a<=b then (galrec (func,true,float_of_int a)) +. (galrec ((SIGMA (INT (a + 1), en, func),x,y)))
      else 0.0
  |INTEGRAL (st, en, func) ->
      let a = galrec (st,x,y) in
      let b = galrec (en,x,y) in
      if a<=b then
	if b-.a>=0.1 then 0.1*.(galrec (func,true,a))+.(galrec (INTEGRAL (REAL (a+.0.1),en,func),x,y))
	else 0.0
      else -.galrec (INTEGRAL (en,st,func),x,y)
	  
let galculator n =
  galrec (n, false, 0.0)
