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
  let rec cal exp x =
    match exp with
    |X -> x
    |INT a -> a
    |ADD (a,b)->(cal a x) + (cal b x)
    |SUB (a,b)->(cal a x) - (cal b x)
    |MUL (a,b)->(cal a x) * (cal b x)
    |DIV (a,b)-> (cal a x) / (cal b x)
    |SIGMA (a,b,c)->
      let a = (cal a x) in let b = (cal b x) in
      if a < b 
      then (cal (ADD(c,INT (cal (SIGMA( (INT (a+1)) ,INT b, c)) x))) a)
      else cal c b 
     in cal exp 0 ;;
