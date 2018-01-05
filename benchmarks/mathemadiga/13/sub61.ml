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

let rec calc(exp,x) =
        match exp with
        | X -> x
        | INT a -> float_of_int a
        | REAL a -> a
        | ADD (a, b) -> (calc(a, x)) +. (calc(b, x))
        | SUB (a, b) -> (calc(a, x)) -. (calc(b, x))
        | MUL (a, b) -> (calc(a, x)) *. (calc(b, x))
        | DIV (a, b) -> (calc(a, x)) /. (calc(b, x))
        | SIGMA (a, b, ftn) ->
                if ((int_of_float (calc(a, x))) = (int_of_float (calc(b, x)))) then calc(ftn, (float_of_int (int_of_float (calc(a, x)))))
                else if ((int_of_float (calc(a, x))) < (int_of_float (calc(b, x)))) then (calc(ftn, (float_of_int (int_of_float (calc(a, x))))) +. (calc(SIGMA(ADD(a, INT 1), b, ftn), x)))
                else 0.0
        | INTEGRAL (a, b, ftn) ->
                if ((calc(a,x) +. 0.1) <= (calc(b,x))) then ((calc(ftn, (calc(a,x))) /. 10.0) +. (calc(INTEGRAL(ADD(a, REAL 0.1), b, ftn),x)))
                else if ((calc(a,x)) <= (calc(b,x))) then 0.0
                else (0.0 -. (calc(INTEGRAL(b, a, ftn), x)))


let rec galculator exp =
        match exp with
        | X -> raise FreeVariable
        | INT a -> float_of_int a
        | REAL a -> a
        | ADD (a, b) -> (galculator a) +. (galculator b)
        | SUB (a, b) -> (galculator a) -. (galculator b)
        | MUL (a, b) -> (galculator a) *. (galculator b)
        | DIV (a, b) -> (galculator a) /. (galculator b)
        | SIGMA (a, b, ftn) ->
                if ((int_of_float (galculator a)) = (int_of_float (galculator b))) then calc(ftn, (float_of_int (int_of_float (galculator a))))
                else if ((int_of_float (galculator a)) < (int_of_float (galculator b))) then (calc(ftn, (float_of_int (int_of_float (galculator a)))) +. (galculator(SIGMA(ADD(a, INT 1), b, ftn))))
                else 0.0
        | INTEGRAL (a, b, ftn) ->
                if (((galculator a) +. 0.1) <= (galculator b)) then ((calc(ftn, (galculator a)) /. 10.0) +. (galculator(INTEGRAL(ADD(a, REAL 0.1), b, ftn))))
                else if ((galculator a) <= (galculator b)) then 0.0
                else (0.0 -. galculator(INTEGRAL(b, a, ftn)))

