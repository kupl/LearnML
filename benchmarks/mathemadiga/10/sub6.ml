(* 컴퓨터공학부 / 2005-11721 / 김재경 / 숙제2-2 *)
type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp;;
exception FreevarError;;
exception DividedByZero;;
exception SigmaDomainError
let rec value(exp, x, x_check) =
    match exp with
      X ->
        if x_check = 0 then raise(FreevarError)
        else x
    | INT(int) -> float_of_int(int)
    | REAL(float) -> float
    | ADD(exp1, exp2) -> value(exp1, x, x_check) +. value(exp2, x, x_check)
    | SUB(exp1, exp2) -> value(exp1, x, x_check) -. value(exp2, x, x_check)
    | MUL(exp1, exp2) -> value(exp1, x, x_check) *. value(exp2, x, x_check)
    | DIV(exp1, exp2) ->
      if value(exp2, x, x_check) = 0. then raise(DividedByZero)
      else value(exp1, x, x_check) /. value(exp2, x, x_check)
    | SIGMA(exp1, exp2, exp3) ->
      let rec sigma_fun(exp3, a, b) =
        if a > b then raise(SigmaDomainError)
        else if a = b then value(exp3, a, 1)
        else value(exp3, a, 1) +. sigma_fun(exp3, a +. 1.0, b) in
      sigma_fun(exp3, value(exp1, x, x_check), value(exp2, x, x_check))
    | INTEGRAL(exp1, exp2, exp3) -> 
      let rec integral_fun(exp3, a, b) =
        if a > b then -.(integral_fun(exp3, b, a))
        else if (a +. 0.1) > b then value(exp3, a, 1) *. (b -. a)
        else (value(exp3, a, 1) *. 0.1) +. integral_fun(exp3, a +. 0.1, b) in
      integral_fun(exp3, value(exp1, x, x_check), value(exp2, x, x_check))
let mathemadiga exp = value(exp, 0., 0)