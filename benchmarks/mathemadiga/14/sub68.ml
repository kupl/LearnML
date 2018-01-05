type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp;;

exception FreeVariable
let rec mygal((funct : exp), x_val) : float =
    match funct with
    X -> x_val
    | INT k -> float_of_int(k)
    | REAL r -> r
    | ADD(n, m) -> (mygal(n, x_val)) +. (mygal(m, x_val))
    | SUB(n, m) -> (mygal(n, x_val)) -. (mygal(m, x_val))
    | MUL(n, m) -> (mygal(n, x_val)) *. (mygal(m, x_val))
    | DIV(n, m) ->
            mygal(n, x_val) /. (mygal(m, x_val))
    | SIGMA(st, en, func) -> 
            let ist = mygal(st, x_val) in
            let ien = mygal(en, x_val) in
            if(int_of_float(ist) > int_of_float(ien)) then 0.0 else
                mygal(func, (float_of_int(int_of_float(ist)))) +. mygal(SIGMA(REAL(ist +. 1.0), REAL ien, func), x_val)
    | INTEGRAL(st, en, func) ->
            let fst = mygal(st, x_val) in
            let fen = mygal(en, x_val) in
            if(fst +. 0.1 > fen && fst <= fen) then 0.0 else
                if(fst > fen) then (-1.0) *. mygal(INTEGRAL(en, st, func), x_val) else
                    mygal(func, fst) /. 10.0 +. mygal(INTEGRAL(REAL(fst +. 0.1), REAL fen, func), x_val)
;;

let rec galculator = function
    X -> raise FreeVariable
    | INT n -> float_of_int(n)
    | REAL r -> r
    | ADD(n, m) -> (galculator(n) +. galculator(m))
    | SUB(n, m) -> (galculator(n) -. galculator(m))
    | MUL(n, m) -> (galculator(n) *. galculator(m))
    | DIV(n, m) -> 
            galculator(n) /. galculator(m)
    | SIGMA(st, en, funct) ->
            let ist = galculator(st) in
            let ien = galculator(en) in
            if(int_of_float(ist) > int_of_float(ien)) then 0.0 else
                mygal(funct, (float_of_int(int_of_float(ist)))) +. galculator(  SIGMA(REAL(float_of_int(int_of_float(ist)) +. 1.0), REAL ien, funct))
    | INTEGRAL(st, en, funct) ->
            let fst = galculator(st) in
            let fen = galculator(en) in
            if(fst +. 0.1 > fen && fst <= fen) then 0.0 else
                if(fst > fen) then (-1.0) *. galculator(INTEGRAL(en, st, funct)) else
                mygal(funct, fst) /. 10.0 +. galculator(INTEGRAL(REAL(fst +. 0.1), REAL fen, funct))
;;
