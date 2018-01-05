exception FreeVariable

type exp =
    | X
    | INT of int
    | REAL of float
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp
    | INTEGRAL of exp * exp * exp
and var =
    | UD
    | INTVAR of int
    | FLOATVAR of float

let evalX (p: var): float = match p with
    | UD -> raise FreeVariable
    | INTVAR i -> float_of_int i
    | FLOATVAR f -> f

let rec galculator(ee: exp): float = 
    let rec galculator_iter (e:exp) (x:var): float = match e with
        | X -> evalX x
        | INT i -> float_of_int i
        | REAL r -> r
        | ADD (p, q) -> galculator_iter p x +. galculator_iter q x
        | SUB (p, q) -> galculator_iter p x -. galculator_iter q x
        | MUL (p, q) -> galculator_iter p x *. galculator_iter q x
        | DIV (p, q) -> galculator_iter p x /. galculator_iter q x
        | SIGMA (p, q, r) -> 
            let rec sigmaIter (m: int) (n: int) (e: exp): float =
                if (m > n)
                then 0.
                else galculator_iter e (INTVAR m) +. sigmaIter (m+1) n e
            in sigmaIter (truncate (galculator_iter p x)) (truncate (galculator_iter q x)) r
        | INTEGRAL (p, q, r) -> 
            let rec integralIter (m: float) (n: float) (e: exp): float =
                if (n -. m > 0.1)
                    then 0.1 *. galculator_iter e (FLOATVAR m) +. integralIter (m+.0.1) n e
                else if (m > n)
                    then -1. *. integralIter n m e 
                else 0. 
            in integralIter (galculator_iter p x) (galculator_iter q x) r
    in galculator_iter ee UD

