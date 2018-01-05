type exp =
    X
    | INT of int
    | REAL of float
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp
    | INTEGRAL of exp * exp * exp

exception FreeVariable

let rec galculator ex =
    match ex with
    X -> (raise FreeVariable)
    | INT a -> (float_of_int a)
    | REAL a -> a
    | ADD (ex1,ex2) -> (galculator ex1) +. (galculator ex2)
    | SUB (ex1,ex2) -> (galculator ex1) -. (galculator ex2)
    | MUL (ex1,ex2) -> (galculator ex1) *. (galculator ex2)
    | DIV (ex1,ex2) -> (galculator ex1) /. (galculator ex2)
    | SIGMA (ex1,ex2,ex3) -> (sigma ( float_of_int(int_of_float(galculator ex1)),
                                    float_of_int(int_of_float(galculator ex2)), fungal ex3) 0.0)
    | INTEGRAL (ex1,ex2,ex3) -> (integral (galculator ex1, galculator ex2, fungal ex3) 0.0)

and sigma (a,b,ex) sum =
    if a > b then 0.0
    else if a = b then sum +. (ex b)
    else sigma (a+.1.0,b,ex) (sum +. (ex a))

and integral (a,b,ex) sum =
    if a > b then -1.0 *. (integral (b, a, ex) sum)
    else if (b -. a) < 0.1 then 0.1 *. sum
    else (integral (a+.0.1, b, ex) (sum +. (ex a)))

and fungal = function
        X -> (fun x -> x)
        | INT a -> (fun x -> float_of_int a)
        | REAL a -> (fun x -> a)
        | ADD (ex1,ex2) -> (fun x -> ((fungal ex1) x) +. ((fungal ex2) x))
        | SUB (ex1,ex2) -> (fun x -> ((fungal ex1) x) -. ((fungal ex2) x))
        | MUL (ex1,ex2) -> (fun x -> ((fungal ex1) x) *. ((fungal ex2) x))
        | DIV (ex1,ex2) -> (fun x -> ((fungal ex1) x) /. ((fungal ex2) x))
        | SIGMA (ex1,ex2,ex3) -> (fun x -> sigma ((fungal ex1) x, (fungal ex2) x, fungal ex3) 0.0)
        | INTEGRAL (ex1,ex2,ex3) -> (fun x -> integral ((fungal ex1) x, (fungal ex2) x, fungal ex3) 0.0)

