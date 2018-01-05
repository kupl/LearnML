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
                let rec sigma(cur, target, acc) =
                    if cur > target then acc
                    else sigma(cur + 1, target, acc +. calculate(e, INT cur))
                in sigma(int_of_float (calculate(b, v)),
                         int_of_float (calculate(t, v)),
                         0.)
        | INTEGRAL (b, t, e) ->
                let rec integral(cur, target, acc) =
                    if (target -. cur) < 0.1 then acc
                    else integral(cur +. 0.1, target, acc +. (calculate(e, REAL cur) *. 0.1))
                in
                let base = calculate(b, v)
                in
                let target = calculate(t, v)
                in
                if base > target then -.integral(target, base, 0.)
                else integral(base, target, 0.)
    in calculate(exp, X)
