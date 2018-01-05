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

let rec calc exp x bind =
        match exp with
          X -> if bind = false then raise FreeVariable
               else x
        | INT(i) -> float_of_int i
        | REAL(f) -> f
        | ADD(e1, e2) -> (calc e1 x bind) +. (calc e2 x bind)
        | SUB(e1, e2) -> (calc e1 x bind) -. (calc e2 x bind)
        | MUL(e1, e2) -> (calc e1 x bind) *. (calc e2 x bind)
        | DIV(e1, e2) -> (calc e1 x bind) /. (calc e2 x bind)
        | SIGMA(e1, e2, e3) -> 
                if (calc e1 x bind) > (calc e2 x bind)      
                 then 0.
                else (calc e3 (float_of_int(int_of_float (calc e1 x bind))) true)
                +. (calc (SIGMA(INT((int_of_float (calc e1 x bind)) + 1),
                INT(int_of_float (calc e2 x bind)), e3)) x bind) 
        | INTEGRAL(e1, e2, e3) -> 
                if (calc e1 x bind) > (calc e2 x bind)
                 then -1. *. (calc (INTEGRAL(e2, e1, e3)) x bind)
                else if (calc e2 x bind) -. (calc e1 x bind) < 0.1
                 then 0.
                else ((calc e3 (calc e1 x bind) true) *. 0.1) +. (calc
                (INTEGRAL(REAL((calc e1 x bind) +. 0.1), REAL(calc e2 x bind),
                e3)) x bind)      

let galculator exp = calc exp 0. false                
