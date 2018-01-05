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

let galculator e =
    let rec eval v f =
        match f with
        | X -> 
            (match v with
            | [] -> raise FreeVariable
            | e :: l -> e)
        | INT i -> float(i)
        | REAL r -> r
        | ADD (a, b) -> (eval v a) +. (eval v b)
        | SUB (a, b) -> (eval v a) -. (eval v b)
        | MUL (a, b) -> (eval v a) *. (eval v b)
        | DIV (a, b) -> (eval v a) /. (eval v b)
        | SIGMA (a, b, f) -> sigma_gal ( int_of_float(eval v a) , int_of_float(eval v b), f, v, 0.0)
        | INTEGRAL (a, b, f) -> if (eval v b) > (eval v a) then integral_gal ((eval v a), (eval v b), f, v, 0.0)
                                else (integral_gal ((eval v b), (eval v a), f, v, 0.0)) *. -1.0
    and sigma_gal (a, b, f, v, result) =
        if a <= b then sigma_gal ( (a + 1), b, f, v, result +. (eval (float_of_int a::v) f) ) else result
    and integral_gal (a, b, f, v, result) = 
        if (b -. a) >= 0.1 then integral_gal ( (a +. 0.1), b, f, v, result +. (eval (a::v) f) *. 0.1 ) else result
    in eval [] e

(*
print_float (galculator (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1))));;
print_endline "";;
print_float (galculator (INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1))));;
print_endline "";;
print_float (galculator (INTEGRAL(INT 0, INT 100000, DIV (SUB(MUL(INT 16, DIV(X, INT 100000)), INT 16),ADD(SUB(MUL(MUL(DIV(X, INT 100000),DIV(X, INT 100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000))), MUL(MUL(INT 2,DIV(X, INT 100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000)))) , SUB(MUL(INT 4, DIV(X, INT 100000)), INT 4) ) ) )) );;
print_endline "";;*)
