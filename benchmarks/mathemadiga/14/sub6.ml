type exp =
  X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception FreeVariable

type option =
  Some of float
| None

let galculator(exp: exp): float =
  let rec galculatorInternal(exp, x: exp * option): float =
    match (exp, x) with
      (X, None) -> raise FreeVariable
    | (X, Some(f)) -> f
    | (INT(i), _) -> float_of_int i
    | (REAL(f), _) -> f
    | (ADD(lh, rh), _) -> galculatorInternal(lh, x) +. galculatorInternal(rh, x)
    | (SUB(lh, rh), _) -> galculatorInternal(lh, x) -. galculatorInternal(rh, x)
    | (MUL(lh, rh), _) -> galculatorInternal(lh, x) *. galculatorInternal(rh, x)
    | (DIV(lh, rh), _) -> galculatorInternal(lh, x) /. galculatorInternal(rh, x)
    | (SIGMA(start, finish, exp), _) ->
        let finish = galculatorInternal(finish, x) in
        let start = galculatorInternal(start, x) in
        let rec sigma(x, result: float * float): float =
          if x > finish
          then result
          else sigma(x +. 1.0, result +. galculatorInternal(exp, Some(x)))
        in sigma(start, 0.0)
    | (INTEGRAL(start, finish, exp), _) ->
        let dx = 0.1 in
        let finish = galculatorInternal(finish, x) in
        let start = galculatorInternal(start, x) in
        if start > finish
        then 0. -. galculatorInternal(INTEGRAL(REAL(finish), REAL(start), exp), x)
        else
          let rec integral(x, result: float * float): float =
            if x +. dx > finish
            then result
            else integral(x +. dx, result +. galculatorInternal(exp, Some(x)) *. dx)
          in integral(start, 0.0)
  in galculatorInternal(exp, None)
