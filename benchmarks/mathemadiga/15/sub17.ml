(* 2010-11753 snucse Taekmin Kim *)
(* HW 2-5 *)

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

let rec galculator: exp -> float = fun(e) ->
  let rec gX: exp * float -> float = fun(e, x) ->
    match e with
    | X -> x
    | INT i -> float_of_int i
    | REAL f -> f
    | ADD (a, b) -> gX(a, x) +. gX(b, x)
    | SUB (a, b) -> gX(a, x) -. gX(b, x)
    | MUL (a, b) -> gX(a, x) *. gX(b, x)
    | DIV (a, b) -> gX(a, x) /. gX(b, x)
    | SIGMA(under, upper, e1) -> galculator(SIGMA(REAL(gX(under, x)), REAL(gX(upper, x)), e1))
    | INTEGRAL(under, upper, e1) -> galculator(SIGMA(REAL(gX(under, x)), REAL(gX(upper, x)), e1))
  in

  match e with
  | X -> raise FreeVariable
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (a, b) -> galculator(a) +. galculator(b)
  | SUB (a, b) -> galculator(a) -. galculator(b)
  | MUL (a, b) -> galculator(a) *. galculator(b)
  | DIV (a, b) -> galculator(a) /. galculator(b)
  | SIGMA(under, upper, e1) -> 
      let un = int_of_float(galculator(under)) in
      let up = int_of_float(galculator(upper)) in
      if un = up then gX(e1, float_of_int(un))
      else if un > up then 0.0
      else gX(e1, float_of_int(un)) +. galculator(SIGMA(ADD(under, INT 1), upper, e1))
  | INTEGRAL(under, upper, e1) -> 
      let un = galculator(under) in
      let up = galculator(upper) in
      if (un >= up && (un -. up) < 0.1) then 0.0
      else if (up >= un && (up -. un) < 0.1) then 0.0
      else if un > up then -1.0 *. galculator(INTEGRAL(upper, under, e1))
      else gX(e1, un) *. 0.1 +. galculator(INTEGRAL(REAL(un +. 0.1), upper, e1))


  (*
let _ = print_endline(string_of_float(galculator X))
let _ = print_endline(string_of_float(galculator (SIGMA (INT 1, INT 10, SUB(MUL(X, X), INT 1)))))
let _ = print_endline(string_of_float(galculator (SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)))))
let _ = print_endline(string_of_float(galculator (INTEGRAL (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)))))
let _ = print_endline(string_of_float(galculator (INTEGRAL (REAL 10., REAL 1., SUB(MUL(X, X), INT 1)))))
let _ = print_endline(string_of_float(galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), INT 1)))))
let _ = print_endline(string_of_float(galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X)))))
let _ = print_endline(string_of_float(galculator (INTEGRAL (ADD (X, REAL 1.), SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X)))))

let a1 = galculator (INTEGRAL(INT 0, INT 100000, DIV (SUB(MUL(INT 16, DIV(X, INT 100000)), INT 16),ADD(SUB(MUL(MUL(DIV(X, INT 100000),DIV(X, INT 100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000))), MUL(MUL(INT 2,DIV(X, INT 100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000)))) , SUB(MUL(INT 4, DIV(X, INT 100000)), INT 4) ) ) )) 
let a2 = galculator(SIGMA(INT 1, INT 1000000, DIV(INT 8, MUL(SUB(MUL(INT 2, X), INT 1),SUB(MUL(INT 2, X), INT 1)))))
let a3 = galculator(SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)))
let a4 = galculator(INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1)))
let _ = print_float(a4)
*)
