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

let rec galculator exp =
    let rec is_function exp =
        match exp with
        | X -> true
        | INT _ -> false
        | REAL _ -> false
        | ADD (l, r) -> (is_function l) || (is_function r)
        | SUB (l, r) -> (is_function l) || (is_function r)
        | MUL (l, r) -> (is_function l) || (is_function r)
        | DIV (l, r) -> (is_function l) || (is_function r)
        | SIGMA (a, b, _) -> (is_function a) || (is_function b)
        | INTEGRAL (a, b, _) -> (is_function a) || (is_function b) in

    let rec substitute_x (exp, a) =
        match exp with
        | X -> a
        | INT i -> float_of_int i
        | REAL r -> r
        | ADD (l, r) -> substitute_x (l, a) +. substitute_x (r, a)
        | SUB (l, r) -> substitute_x (l, a) -. substitute_x (r, a)
        | MUL (l, r) -> substitute_x (l, a) *. substitute_x (r, a)
        | DIV (l, r) -> substitute_x (l, a) /. substitute_x (r, a)
        | SIGMA (l, r, f) -> galculator (SIGMA (REAL (substitute_x (l, a)), REAL (substitute_x (r, a)), f))
        | INTEGRAL (l, r, f) -> galculator (INTEGRAL (REAL (substitute_x (l, a)), REAL (substitute_x (r, a)), f)) in

    let rec replace_x (exp, a) =
        match exp with
        | X -> a
        | INT _ -> exp
        | REAL _ -> exp
        | ADD (l, r) -> ADD (replace_x (l, a), replace_x (r, a))
        | SUB (l, r) -> SUB (replace_x (l, a), replace_x (r, a))
        | MUL (l, r) -> MUL (replace_x (l, a), replace_x (r, a))
        | DIV (l, r) -> DIV (replace_x (l, a), replace_x (r, a))
        | SIGMA (l, r, f) -> SIGMA (replace_x (l, a), replace_x (r, a), f)
        | INTEGRAL (l, r, f) -> INTEGRAL (replace_x (l, a), replace_x (r, a), f) in


    match exp with
    | X -> raise FreeVariable
    | INT i -> float_of_int i
    | REAL r -> r
    | ADD (l, r) -> (galculator l) +. (galculator r)
    | SUB (l, r) -> (galculator l) -. (galculator r)
    | MUL (l, r) -> (galculator l) *. (galculator r)
    | DIV (l, r) -> (galculator l) /. (galculator r)
    | SIGMA (a, b, f) -> if (is_function f) = false then galculator (MUL ((ADD (SUB (b, a), INT 1)), f))
                    else if int_of_float (galculator a) > int_of_float (galculator b) then 0.0
                    else if int_of_float (galculator a) = int_of_float (galculator b) then substitute_x (f, float_of_int (int_of_float (galculator a)))
                    else substitute_x (f, float_of_int (int_of_float (galculator b))) +. (galculator (SIGMA (a, SUB (b, INT 1), f)))
    | INTEGRAL (a, b, f) -> if (galculator a)>(galculator b) then -.galculator (INTEGRAL (b, a, f))
                        else if (is_function f) = false then galculator (MUL (SUB (b, a), f))
						else if (galculator b) -. (galculator a) < 0.1 then 0.0
						else (substitute_x (f, (galculator a)) *. 0.1) +. galculator (INTEGRAL (ADD (a, REAL 0.1), b, f)) 

                      

