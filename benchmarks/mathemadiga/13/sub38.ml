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

let rec apply (f, p) =
    match f with
    | X -> p
    | INT _ -> f
    | REAL _ -> f
    | ADD(a, b) -> ADD(apply (a, p), apply (b, p))
    | SUB(a, b) -> SUB(apply (a, p), apply (b, p))
    | MUL(a, b) -> MUL(apply (a, p), apply (b, p))
    | DIV(a, b) -> DIV(apply (a, p), apply (b, p))
    | SIGMA(a, b, f) -> if int_of_float (galculator a) > int_of_float (galculator b)
                        then REAL 0.0
                        else ADD ((apply (f, (INT (int_of_float (galculator a)))), SIGMA (ADD (INT (int_of_float (galculator a)), INT 1), b, f)))
    | INTEGRAL(a, b, f) -> if (galculator a) > (galculator b) -. 0.1
                           then REAL 0.0
                           else ADD (MUL (REAL 0.1, (apply (f, REAL (galculator a)))), INTEGRAL (ADD (REAL 0.1, REAL (galculator a)), b, f))
and galculator e =
    match e with
    | X -> raise FreeVariable
    | INT i -> float_of_int i
    | REAL f -> f
    | ADD(a, b) -> (galculator a) +. (galculator b)
    | SUB(a, b) -> (galculator a) -. (galculator b)
    | MUL(a, b) -> (galculator a) *. (galculator b)
    | DIV(a, b) -> (galculator a) /. (galculator b)
    | SIGMA(a, b, f) -> if int_of_float (galculator a) > int_of_float (galculator b)
                        then 0.0 
                        else galculator (ADD (apply (f, (INT (int_of_float (galculator a)))), SIGMA (ADD (INT (int_of_float (galculator a)), INT 1), b, f)))
    | INTEGRAL(a, b, f) -> if (galculator a) > (galculator b) then galculator (INTEGRAL(b, a, f)) else
                           (if (galculator a) > (galculator b) -. 0.1
                           then 0.0
                           else galculator (ADD (MUL (REAL 0.1, (apply (f, REAL (galculator a)))), INTEGRAL (ADD (REAL 0.1, REAL (galculator a)), b, f))))
