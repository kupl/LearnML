type exp = X 
  | INT  of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculater : exp -> int
= fun e ->
  let rec incalculate : int -> exp -> int
  = fun xval e1 ->
    match e1 with
    | X -> xval
    | INT (i) -> i
    | ADD (x, p) -> (incalculate xval x) + (incalculate xval p)
    | SUB (x, p) -> (incalculate xval x) - (incalculate xval p)
    | MUL (x, p) -> (incalculate xval x) * (incalculate xval p)
    | DIV (x, p) -> (incalculate xval x) / (incalculate xval p)
    | SIGMA (st, des, expression) -> if (calculater st) > (calculater des) then 0 
      else (incalculate (calculater st) expression) + (incalculate (calculater st) (SIGMA (INT((calculater st)+1), des, expression)))
  in
  match e with
  | X -> 1
  | INT (i) -> i
  | ADD (x, p) -> calculater x + calculater p
  | SUB (x, p) -> calculater x - calculater p
  | MUL (x, p) -> calculater x * calculater p
  | DIV (x, p) -> calculater x / calculater p
  | SIGMA (st, des, expression) -> if (calculater st) >(calculater des) then 0
    else incalculate (calculater st) (SIGMA(INT ((calculater st) + 1), des, expression));;
