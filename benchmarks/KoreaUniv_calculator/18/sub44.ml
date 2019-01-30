type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp


let rec sigmaf p q r f = if p <> q then f p r + sigmaf (p+1) q r f else f p r;;

let rec temp m exp = match exp with
  | X -> m
  | INT a -> a
  | ADD(a,b) -> (temp m a) + (temp m b)
  | SUB(a,b) -> (temp m a) - (temp m b)
  | MUL(a,b) -> (temp m a) * (temp m b)
  | DIV(a,b) -> (temp m a) / (temp m b)
  | SIGMA(a,b,c) -> sigmaf (temp 0 a) (temp 0 b) c temp;;

let rec calculator : exp -> int
= fun exp -> temp 0 exp;;
