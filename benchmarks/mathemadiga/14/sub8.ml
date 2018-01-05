type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp;;

exception FreeVariable;;

let rec galculator_helper exp bind =
    match exp with
        | X -> ((function [] -> raise FreeVariable | h::t -> h) bind)
        | INT x -> (float_of_int x)
        | REAL x -> x
        | ADD(exp1, exp2) -> (galculator_helper exp1 bind) +. (galculator_helper exp2 bind)
        | SUB(exp1, exp2) -> (galculator_helper exp1 bind) -. (galculator_helper exp2 bind)
        | MUL(exp1, exp2) -> (galculator_helper exp1 bind) *. (galculator_helper exp2 bind)
        | DIV(exp1, exp2) -> (galculator_helper exp1 bind) /. (galculator_helper exp2 bind)
        | SIGMA(exp1, exp2, exp3) ->
            let first = (int_of_float (galculator_helper exp1 [])) and last = (int_of_float (galculator_helper exp2 [])) in
            if first > last then 0.0
            else if first = last
                then (galculator_helper exp3 [float_of_int(first)])
                else (galculator_helper exp3 [float_of_int(first)]) +. (galculator_helper (SIGMA ((INT (1 + first)), (INT last), exp3)) [])
        | INTEGRAL(exp1, exp2, exp3) ->
            let first = (galculator_helper exp1 []) and last = (galculator_helper exp2 []) in
            if first > last
                then (0.0 -. (galculator_helper (INTEGRAL (exp2, exp1, exp3)) bind))
                else if last -. first < 0.1
                    then 0.0
                    else ((galculator_helper exp3 [first]) *. 0.1) +. (galculator_helper (INTEGRAL ((REAL (0.1 +. first)), REAL( last), exp3)) []);;

let rec galculator exp =
    galculator_helper exp [];;
(*
print_endline (string_of_float (galculator (SIGMA (INT 1, INT 10, SUB(MUL(X, X), INT 1)))));;
print_endline (string_of_float (galculator (SIGMA (REAL 1.1, REAL 10.9, SUB(MUL(X, X), INT 1)))));;
print_endline (string_of_float (galculator (INTEGRAL (REAL 1., REAL 1000., SUB(MUL(X, X), INT 1)))));;
print_endline (string_of_float (galculator (INTEGRAL (REAL 10., REAL 1., SUB(MUL(X, X), INT 1)))));;
print_endline (string_of_float (galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), INT 1)))));;
print_endline (string_of_float (galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X)))));;
print_endline (string_of_float (galculator (INTEGRAL (ADD (X, REAL 1.), SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X)))));;
*)
