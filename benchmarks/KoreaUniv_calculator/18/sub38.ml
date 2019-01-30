type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec inse :exp -> exp -> exp
= fun exp x-> match exp with
  X -> x
  |INT a -> INT a 
  |ADD (INT a, INT b) -> INT (a+b)
  |ADD (a, b) -> inse (ADD (inse a x, inse b x)) x
  |SUB (INT a, INT b) -> INT (a-b)
  |SUB (a, b) -> inse (SUB (inse a x, inse b x)) x
  |MUL (INT a, INT b) -> INT (a*b)
  |MUL (a, b) -> inse (MUL (inse a x, inse b x)) x
  |DIV (INT a, INT b) -> INT (a/b)
  |DIV (a, b) -> inse (DIV (inse a x, inse b x)) x
  |SIGMA (INT a, INT b, c) -> if a < b then ADD (inse c (INT a) , inse (SIGMA (INT (a+1), INT b, c)) (INT (a+1))) else inse c (INT a)
  |SIGMA (_, _, _) -> raise (Failure "fail");;

let rec calculator : exp -> int
= fun exp -> match exp with
  X -> 0
  |INT a -> a
  |ADD (INT a, INT b) -> a+b
  |ADD (a, b) -> calculator (inse (ADD (inse a (INT 0), inse b (INT 0))) (INT 0))
  |SUB (INT a, INT b) -> a-b
  |SUB (a, b) -> calculator (inse (SUB (inse a (INT 0), inse b (INT 0))) (INT 0))
  |MUL (INT a, INT b) -> a*b
  |MUL (a, b) -> calculator (inse (MUL (inse a (INT 0), inse b (INT 0))) (INT 0))
  |DIV (INT a, INT b) -> a/b
  |DIV (a, b) -> calculator (inse (DIV (inse a (INT 0), inse b (INT 0))) (INT 0))
  |SIGMA (INT a, INT b, c) -> if a < b then calculator (ADD (inse c (INT a) , inse (SIGMA (INT (a+1), INT b, c)) (INT (a+1)))) else calculator (inse c (INT a))
  |SIGMA (_, _, _) -> 0;;
  