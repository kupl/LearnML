exception FreeVariable;;

type exp =
      X
    | INT of int
    | REAL of float
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp
    | INTEGRAL of exp * exp * exp;;

let rec galculator e = 
  match e with
  | X -> raise FreeVariable
  | INT z -> float_of_int z
  | REAL r -> r
  | ADD (e1,e2) -> (galculator e1) +. (galculator e2)
  | SUB (e1,e2) -> (galculator e1) -. (galculator e2)
  | MUL (e1,e2) -> (galculator e1) *. (galculator e2)
  | DIV (e1,e2) -> (galculator e1) /. (galculator e2)
  | SIGMA (i,j,e) -> 
      let ii = int_of_float (galculator i) and jj = int_of_float (galculator j) in
      if ii>jj then 0. else eval (e,i) +. galculator (SIGMA (ADD(i,INT 1),j,e))
  | INTEGRAL (i,j,e) -> 
      let ii = (galculator i) and jj = (galculator j) and dx=0.1 in
      if ii>jj then -.galculator (INTEGRAL (j,i,e))
      else if jj-.ii < dx then 0. else eval(e,i)*.dx +. galculator (INTEGRAL (ADD(i,REAL dx),j,e))
and eval (f,x) = 
  match f with
  | X -> galculator x
  | INT z -> float_of_int z
  | REAL r -> r
  | ADD (f1,f2) -> eval (f1,x) +. eval (f2,x)
  | SUB (f1,f2) -> eval (f1,x) -. eval (f2,x)
  | MUL (f1,f2) -> eval (f1,x) *. eval (f2,x)
  | DIV (f1,f2) -> eval (f1,x) /. eval (f2,x)
  | SIGMA (i,j,f1) -> galculator (SIGMA (REAL (eval (i,x)), REAL (eval (j,x)), f1))
  | INTEGRAL (i,j,f1) -> galculator (INTEGRAL (REAL (eval(i,x)), REAL (eval(j,x)), f1));;
