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


let rec galculator: exp -> float = fun(e) ->
    let rec func: (exp * float) -> float = fun(e, x) ->
        match e with
        | X -> x
        | INT(a) -> float_of_int a
        | REAL(a) -> a
        | ADD(a, b) -> func(a, x) +. func(b, x)
        | SUB(a, b) -> func(a, x) -. func(b, x)
        | MUL(a, b) -> func(a, x) *. func(b, x)
        | DIV(a, b) -> func(a, x) /. func(b, x)
        | SIGMA(a, b, c) -> galculator(SIGMA(REAL(func(a, x)), REAL(func(b,x)), c))
        | INTEGRAL(a, b, c) -> galculator(INTEGRAL(REAL(func(a, x)),
        REAL(func(b, x)), c)) 
    in
        
    let rec sigma: (float*int*int*exp) -> float = fun(cur, a, b, e) ->
        if a > b then cur
        else sigma(cur +. func(e, float_of_int(a)), a+1, b, e)
    in

    let rec integral: (float*float*float*exp) -> float = fun(cur, a, b, e) ->
        if a > b then -. integral(cur, b, a, e)
        else if b -. a < 0.1 then cur
        else integral(cur +. (0.1 *. func(e, a)), a +. 0.1, b, e)
    in

    match e with
    | X -> raise FreeVariable
    | INT(a) -> float_of_int a
    | REAL(a) -> a
    | ADD(a, b) -> galculator(a) +. galculator(b)
    | SUB(a, b) -> galculator(a) -. galculator(b)
    | MUL(a, b) -> galculator(a) *. galculator(b)
    | DIV(a, b) -> galculator(a) /. galculator(b)
    | SIGMA(a, b, c) -> sigma(0.0, int_of_float(galculator(a)),
    int_of_float(galculator(b)), c)
    | INTEGRAL(a, b, c) -> integral(0.0, galculator(a), galculator(b), c) 
   
(*
let _ = print_endline(string_of_float (galculator (INTEGRAL(INT 0, INT 100000, DIV (SUB(MUL(INT 16, DIV(X, INT
100000)), INT 16),ADD(SUB(MUL(MUL(DIV(X, INT 100000),DIV(X, INT
100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000))), MUL(MUL(INT 2,DIV(X, INT
100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000)))) , SUB(MUL(INT 4, DIV(X,
    INT 100000)), INT 4) ) ) )) ))


let _ = print_endline(string_of_float (galculator(SIGMA(INT 1, INT 1000000,
DIV(INT 8, MUL(SUB(MUL(INT 2, X), INT 1),SUB(MUL(INT 2, X), INT 1))))) 
))
*)
