type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> 
  let rec cal_x exp num =
    match exp with
      X -> num
      |INT n -> n
      |ADD (x, y) -> cal_x x num + cal_x y num
      |SUB (x, y) -> cal_x x num - cal_x y num
      |MUL (x, y) -> cal_x x num * cal_x y num
      |DIV (x, y) -> cal_x x num / cal_x y num
      |SIGMA (first, last, func) -> 
        if cal_x first num <= cal_x last num then
          cal_x (ADD(INT (cal_x func (cal_x first num)),SIGMA(ADD(first,INT 1), last, func))) num
        else 
          0
  in cal_x exp 0;;
  