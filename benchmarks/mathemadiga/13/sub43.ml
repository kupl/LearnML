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

let galculator exp =
    let rec calculate(e, v) =
        match e with
        | X -> (match v with
                      | X -> raise FreeVariable
                      | value -> calculate(value, X))
        | INT n -> float_of_int n
        | REAL n -> n
        | ADD (x, y) -> calculate(x, v) +. calculate(y, v)
        | SUB (x, y) -> calculate(x, v) -. calculate(y, v)
        | MUL (x, y) -> calculate(x, v) *. calculate(y, v)
        | DIV (x, y) -> calculate(x, v) /. calculate(y, v)
        | SIGMA (b, t, e) ->
                let base = (int_of_float (calculate(b, v))) in
                let target = (int_of_float (calculate(t, v))) in
                if base > target then 0.
                else calculate(ADD (REAL (calculate(e, INT base)), SIGMA (INT (base + 1), INT target, e)), v)
        | INTEGRAL (b, t, e) ->
                let base = calculate(b, v) in
                let target = calculate(t, v) in
                if base > target then -.calculate(INTEGRAL (t, b, e), v)
                else if (target -. base) < 0.1 then 0.
                else calculate(ADD (REAL (calculate(e, REAL base) *. 0.1), INTEGRAL (REAL (base +. 0.1), REAL target, e)), v)
    in calculate(exp, X)
