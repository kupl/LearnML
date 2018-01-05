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

let rec sigma (a, b, f) =
    if (a > b) then 0.0 
    else if (a = b) then (f a)
    else (f a) +. (sigma (a+1, b, f))

let rec integral (a, b, f) =
    if (a > b) then (-1.0 *. (integral (b, a, f)))
    else if ((b -. a) < 0.1) then 0.0
    else ((f a) *. 0.1) +. (integral (a+.0.1, b, f))

let rec 
    gal_sigma f a =
        match f with
        | X -> float_of_int a
        | INT n -> float_of_int n
        | REAL n -> n
        | ADD (n1, n2) -> (gal_sigma n1 a) +. (gal_sigma n2 a)
        | SUB (n1, n2) -> (gal_sigma n1 a) -. (gal_sigma n2 a)
        | MUL (n1, n2) -> (gal_sigma n1 a) *. (gal_sigma n2 a)
        | DIV (n1, n2) -> (gal_sigma n1 a) /. (gal_sigma n2 a)
        | SIGMA (n1, n2, e) -> sigma ((int_of_float (gal_sigma n1 a)), (int_of_float (gal_sigma n2 a)), (gal_sigma e))
        | INTEGRAL (n1, n2, e) -> integral ((gal_sigma n1 a), (gal_sigma n2 a), (gal_integral e))
and 
   gal_integral f a =
       match f with
       | X -> a
       | INT n -> float_of_int n 
       | REAL n -> n
       | ADD (n1, n2) -> (gal_integral n1 a) +. (gal_integral n2 a)
       | SUB (n1, n2) -> (gal_integral n1 a) -. (gal_integral n2 a)
       | MUL (n1, n2) -> (gal_integral n1 a) *. (gal_integral n2 a)
       | DIV (n1, n2) -> (gal_integral n1 a) /. (gal_integral n2 a)
       | SIGMA (n1, n2, e) -> sigma ((int_of_float (gal_integral n1 a)), (int_of_float (gal_integral n2 a)), (gal_sigma e))
       | INTEGRAL (n1, n2, e) -> integral ((gal_integral n1 a), (gal_integral n2 a), (gal_integral e))

let rec galculator exp =
    match exp with
    | X -> raise FreeVariable
    | INT n -> float_of_int n
    | REAL n -> n
    | ADD (n1, n2) -> (galculator n1) +. (galculator n2)
    | SUB (n1, n2) -> (galculator n1) -. (galculator n2)
    | MUL (n1, n2) -> (galculator n1) *. (galculator n2)
    | DIV (n1, n2) -> (galculator n1) /. (galculator n2)
    | SIGMA (n1, n2, e) -> sigma ((int_of_float (galculator n1)), (int_of_float (galculator n2)), (gal_sigma e))
    | INTEGRAL (n1, n2 ,e) -> integral ((galculator n1), (galculator n2), (gal_integral e))
