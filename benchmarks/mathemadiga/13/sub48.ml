type exp = X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp ;;

type environment =
  | NONE
  | X_ENV of float;;

exception FreeVariable;;
let rec sigma (beg, ed, f) =
  if beg > ed then 0.
  else f(ed) +. (sigma(beg, ed-1, f));;

let rec integrate (beg, ed, f, dx) =
  if beg >= ed then 0.
  else (f(beg)) *. dx +. (integrate(beg +. dx, ed, f, dx)) ;;

let rec eval (expr, env) =
  match expr with
  | X ->
    (match env with
      | NONE -> raise FreeVariable
      | X_ENV f -> f)
  | INT i -> float_of_int(i)
  | REAL f -> f
  | ADD (exp1, exp2) -> (eval(exp1, env)) +. (eval(exp2, env))
  | SUB (exp1, exp2) -> (eval(exp1, env)) -. (eval(exp2, env))
  | MUL (exp1, exp2) -> (eval(exp1, env)) *. (eval(exp2, env))
  | DIV (exp1, exp2)  -> (eval(exp1, env)) /. (eval(exp2, env))
  | SIGMA (beg, ed, expr)  -> sigma((int_of_float(eval(beg, env))), (int_of_float(eval(ed, env))), (fun x -> eval(expr, X_ENV(float_of_int(x)))))
  | INTEGRAL (beg, ed, expr) -> integrate((eval(beg, env)), (eval(ed, env)), (fun x -> eval(expr, X_ENV(x))), 0.1);;
let galculator (expr) =
  eval(expr, NONE) ;;

assert(galculator(ADD(INT(2),  REAL(3.))) = 5.);;
assert(galculator(SUB(INT(2),  REAL(3.))) = -1.);;
assert(galculator(MUL(INT(2), REAL(3.))) = 6.);;
assert(galculator(DIV(INT(12), REAL(3.))) = 4.);;
assert(galculator(SIGMA(INT(0), REAL(10.), X)) = 55.);;
assert(galculator(SIGMA(INT(0), REAL(10.), MUL(X, X))) = 385.);;
assert(galculator(SIGMA(INT(5), REAL(10.), MUL(MUL(X, X), X))) = 2925.);;
