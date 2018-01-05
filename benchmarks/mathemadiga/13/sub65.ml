type exp=X
  | INT of int
  | REAL of float
  | ADD of exp*exp
  | SUB of exp*exp
  | MUL of exp*exp
  | DIV of exp*exp
  | SIGMA of exp*exp*exp
  | INTEGRAL of exp*exp*exp

  exception FreeVariable
  let rec galcX (expf, fl) =
match expf with
  X -> fl
  |INT n -> float_of_int n
  |REAL f -> f
  |ADD (exp1, exp2) -> galcX(exp1,fl) +. galcX(exp2,fl)
  |SUB (exp1, exp2) -> galcX(exp1,fl) -. galcX(exp2,fl)
  |MUL (exp1, exp2) -> galcX(exp1,fl) *. galcX(exp2,fl)
  |DIV (exp1, exp2) -> (galcX(exp1,fl)/.galcX(exp2,fl))
  |SIGMA (exp1, exp2, expf) ->
    if(galcX(exp1,fl)>galcX(exp2,fl)) then 0. 
  else if (galcX(exp1,fl)=galcX(exp2,fl))
  then galcX(expf, galcX(exp1,fl))
  else (galcX(SIGMA(exp1, SUB(exp2, INT 1), expf), galcX(SUB(exp2, INT 1),fl))+.galcX(expf, galcX(exp2,fl)))
  |INTEGRAL(exp1, exp2, expf)->
    if(galcX(exp1,fl)=galcX(exp2,fl)) then 0.
    else if(galcX(exp1,fl)>galcX(exp2,fl)) then 0.-.galcX(INTEGRAL(exp2, exp1, expf), fl)
  else (galcX(INTEGRAL(exp1, SUB(exp2, REAL 0.1), expf), fl)+. 0.1*.galcX(expf, galcX(exp2, fl)))


  let rec galculator iniexp =
  match iniexp with
  X -> raise FreeVariable
  |INT n -> float_of_int n
  |REAL f -> f
  |ADD (exp1, exp2) -> galculator(exp1) +. galculator(exp2)
  |SUB (exp1, exp2) -> galculator(exp1) -. galculator(exp2)
  |MUL (exp1, exp2) -> galculator(exp1) *. galculator(exp2)
  |DIV (exp1, exp2) -> (galculator(exp1)/.galculator(exp2))
  |SIGMA (exp1, exp2, expf) ->
    if(galculator(exp1)>galculator(exp2)) then 0. 
  else if (galculator(exp1)=galculator(exp2)) then galcX(expf, galculator(exp1))
  else (galculator(SIGMA(exp1, SUB(exp2, INT 1), expf))+. galcX(expf, galculator(exp2)))
  |INTEGRAL(exp1, exp2, expf)->
    if(galculator(exp1)=galculator(exp2)) then 0. 
    else if(galculator(exp1)>galculator(exp2)) then 0.-.galculator(INTEGRAL(exp2, exp1, expf))
  else (galculator(INTEGRAL(exp1, SUB(exp2, REAL 0.1), expf))+. 0.1*.galcX(expf, galculator(exp2)))