
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

exception Problem

let rec cal (exp, num) =
 match exp with
 | X -> num
 | INT (n)-> n
 | ADD (exp1, exp2) -> (cal (exp1, num)) + (cal (exp2, num))
 | SUB (exp1, exp2) -> (cal (exp1, num)) - (cal (exp2, num))
 | MUL (exp1, exp2) -> (cal (exp1, num)) * (cal (exp2, num))
 | DIV (exp1, exp2) -> (cal (exp1, num)) / (cal (exp2, num))
 | SIGMA (exp1, exp2, exp3) ->
 	(match calculator (exp1) with
 		m ->if m=calculator (exp2) then cal (exp3, m) else cal (exp3, m) + cal (SIGMA(INT (m+1), exp2, exp3),m))

and calculator : exp -> int
= fun exp ->
match exp with 
X -> raise Problem
|INT (n) -> n
|ADD (exp1, exp2) -> calculator (exp1) + calculator (exp2)
|SUB (exp1, exp2) -> calculator (exp1) - calculator (exp2)
|MUL (exp1, exp2) -> calculator (exp1) * calculator (exp2)
|DIV (exp1, exp2) -> calculator (exp1) / calculator (exp2)
|SIGMA (exp1, exp2, exp3) -> 
	(match calculator (exp1) with
		m -> if m=calculator (exp2) then cal (exp3, m) else cal (exp3, m) + calculator (SIGMA (INT (m+1), exp2 , exp3)));;
