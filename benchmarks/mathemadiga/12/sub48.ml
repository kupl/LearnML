type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception Error

let mathemadiga exp =
    let rec eval exp var = match exp with
    | X -> if var == nan then raise Error else var
    | INT i -> float_of_int(i)
    | REAL r -> r
    | ADD(e1, e2) -> eval e1 var +. eval e2 var
    | SUB(e1, e2) -> eval e1 var -. eval e2 var
    | MUL(e1, e2) -> eval e1 var *. eval e2 var
    | DIV(e1, e2) -> eval e1 var /. eval e2 var
    | SIGMA(l, h, e) ->
            let low = int_of_float(eval l var) in
            let high = int_of_float(eval h var) in
            if low > high then 0.0
            else (eval e (float_of_int(low))) +.
                (eval (SIGMA(INT(low+1), h, e)) var)
    | INTEGRAL(l, h, e) ->
            let low = (eval l var) in
            let high = (eval h var) in
            if low > high then
                if low -. high < 0.1 then 0.0
                else eval (MUL(INT(-1), INTEGRAL(h, l, e))) var
            else (eval (MUL(e, REAL(0.1))) low) +.
                (eval (INTEGRAL(REAL(low+.0.1), h, e)) var)
    in eval exp nan
