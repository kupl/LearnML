type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec cal_sig : exp -> int -> int
= fun exp i ->
  match exp with
      | X -> i
      | INT n -> n
      | ADD (a, b) -> (cal_sig a i) + (cal_sig b i)
      | SUB (a, b) -> (cal_sig a i) - (cal_sig b i)
      | MUL (a, b) -> (cal_sig a i) * (cal_sig b i)
      | DIV (a, b) -> (cal_sig a i) / (cal_sig b i)
      | SIGMA (a, b, c) ->
        if a > b then 0
        else ((cal_sig c (cal_sig a 0)) + cal_sig (SIGMA(INT ((cal_sig a 0)+1), b, c)) 0);;
        
let rec calculator : exp -> int
= fun exp ->
   cal_sig exp 0;;
  