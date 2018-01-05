exception FreevarError
exception Error of string
exception DividedByZero

type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let rec mathe_inter exp lst= match exp with
X ->  if (lst=[]) then (raise FreevarError)
      else (List.nth lst 0)
| INT(value) -> ((float_of_int) value)
| REAL(value) -> value
| ADD (exp1, exp2) -> ((mathe_inter exp1 lst) +. (mathe_inter exp2 lst))
| SUB (exp1, exp2) -> ((mathe_inter exp1 lst) -. (mathe_inter exp2 lst))
| MUL (exp1, exp2) -> ((mathe_inter exp1 lst) *. (mathe_inter exp2 lst))
| DIV (exp1, exp2) -> (
                        let dv = (mathe_inter exp2 lst) in
                        if (dv=0.0) then  (raise DividedByZero)
                                    else  ((mathe_inter exp1 lst) /. (dv))
                      )
| SIGMA (INT(val1), INT(val2), exp3) -> if (val1>val2) then 0.0 
                                       else 
                                       ((mathe_inter exp3 [(float_of_int)val1]) +.
                                       (mathe_inter (SIGMA (INT(val1+1), INT(val2), exp3)) lst))
| SIGMA (_, _, _) -> (raise (Error "SIGMA type error"))
| INTEGRAL (REAL(val1), REAL(val2), exp3) -> if(val1=val2) then 0.0 
                                       else if(val1>val2) then ((-1.0) *. (mathe_inter (INTEGRAL (REAL(val2), REAL(val1), exp3)) lst))
                                       else if((val2-.val1)<= 0.1) then  ((mathe_inter exp3 [val1]) *. (val2-.val1))
                                       else (((mathe_inter exp3 [val1]) *. 0.1) +.
                                       (mathe_inter (INTEGRAL (REAL(val1 +. 0.1), REAL(val2), exp3)) lst))                                     
| INTEGRAL (exp1, exp2, exp3) -> (mathe_inter (INTEGRAL (REAL((mathe_inter exp1 lst)), REAL((mathe_inter exp2 lst)), exp3)) lst)

let mathemadiga exp = mathe_inter exp []