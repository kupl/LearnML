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

let rec galculator (e: exp): float =
  let rec exp_func (e: exp) (i: float): float =
    match e with
    | X -> i
    | INT e1 -> float_of_int e1
    | REAL e1 -> e1
    | ADD (e1, e2) -> (exp_func e1 i) +. (exp_func e2 i)
    | SUB (e1, e2) -> (exp_func e1 i) -. (exp_func e2 i)
    | MUL (e1, e2) -> (exp_func e1 i) *. (exp_func e2 i)
    | DIV (e1, e2) -> (exp_func e1 i) /. (exp_func e2 i)
    | SIGMA _ -> galculator e
    | INTEGRAL _ -> galculator e
  in
  match e with
  | X -> raise FreeVariable
  | INT e1 -> float_of_int e1
  | REAL e1 -> e1
  | ADD (e1, e2) -> (galculator e1) +. (galculator e2)
  | SUB (e1, e2) -> (galculator e1) -. (galculator e2)
  | MUL (e1, e2) -> (galculator e1) *. (galculator e2)
  | DIV (e1, e2) -> (galculator e1) /. (galculator e2)
  | SIGMA (e1, e2, e3) -> let e1_float = (galculator e1) in
                          let e2_float = (galculator e2) in
                          let e1_int = int_of_float e1_float in
                          let e2_int = int_of_float e2_float in
                          if e1_int < e2_int
                            then galculator (SIGMA (INT (e1_int + 1), INT e2_int, e3)) +. (exp_func e3 e1_float)
                          else if e1_int == e2_int
                            then exp_func e3 e1_float
                            else 0.
  | INTEGRAL (e1, e2, e3) -> let e1_float = galculator e1 in
                             let e2_float = galculator e2 in
                             let range = e2_float -. e1_float in
                             if range >= 0.1
                               then galculator (INTEGRAL (REAL (e1_float +. 0.1), REAL e2_float, e3)) +. (0.1 *. (exp_func e3 e1_float))
                             else if range <= -0.1
                              then galculator (INTEGRAL (REAL (e1_float -. 0.1), REAL e2_float, e3)) +. (0.1 *. (exp_func e3 e1_float))
                              else 0.
