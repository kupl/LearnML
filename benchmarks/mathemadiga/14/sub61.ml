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
         ;;

let rec calculator (exp, value) =
    match exp with
    |X -> value
    |INT n -> float_of_int n
    |REAL r -> r
    |ADD (a,b) -> (calculator (a, value)) +. (calculator (b, value))
    |SUB (a,b) -> (calculator (a, value)) -. (calculator (b, value))
    |MUL (a,b) -> (calculator (a, value)) *. (calculator (b, value))
    |DIV (a,b) -> (calculator (a, value)) /. (calculator (b, value))
    |SIGMA (first, last, expr) -> 
        if ((calculator (SUB(first,last), 0.0)) > 0.0) then 0.0
        else (calculator (expr, calculator(first, 0.0))) +.
        calculator(SIGMA(ADD(first,INT 1),last,expr),0.0)
    |INTEGRAL (first, last, expr) ->
        if (calculator (SUB(first,last), 0.0)) > 0.0 then calculator(INTEGRAL
        (last, first,expr), 0.0)
        else if ((calculator (SUB(last, first), 0.0)) < 0.1) then 0.0
        else (calculator (expr, calculator(first, 0.0))) *. 0.1 +. calculator(INTEGRAL (ADD(first, REAL
        0.1),last, expr), 0.0)
;;
let galculator exp = calculator (exp, 0.0)
;;
