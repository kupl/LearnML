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
;;

exception FreeVariable

let rec galculator exp =
  let rec galc exp x =
    match exp with
    | X -> x
    | INT i -> float_of_int i
    | REAL i -> i
    | ADD (e1, e2) -> galc e1 x +. galc e2 x
    | SUB (e1, e2) -> galc e1 x -. galc e2 x
    | MUL (e1, e2) -> galc e1 x *. galc e2 x
    | DIV (e1, e2) -> galc e1 x /. galc e2 x
    | SIGMA (e1, e2, e3) ->
        if (galc e1 x) <= (galc e2 x) then
          (galc e3 (galc e1 x)) +. (galc (SIGMA (REAL ((galc e1 x) +. 1.), e2, e3)) x)
        else
          0.
    | INTEGRAL (e1, e2, e3) ->
        if (galc e1 x) > ((galc e2 x) +. 0.1) then
          (galc (SUB (REAL 0. , INTEGRAL (e2, e1, e3))) x)
        else if (galc e1 x) < (galc e2 x) then
          (galc e3 (galc e1 x)) *. 0.1 +. (galc (INTEGRAL (REAL ((galc e1 x) +. 0.1), e2, e3)) x)
        else
          0.
  in galc exp 0.
;;

(* TEST CASE *)

(* 
let _ =
  let e1 = SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)) in 
  let e2 = INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X,X), INT 1)) in 
  let e3 = INTEGRAL(REAL 10.0, REAL 1.0, SUB(MUL(X,X), INT 1)) in 
  print_float (galculator e1); print_endline "";
  print_float (galculator e2); print_endline "";
  print_float (galculator e3); print_endline "";

*)
