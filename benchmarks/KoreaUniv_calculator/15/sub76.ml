(* 2011210039 Kang Seungwoo *)

exception E;;

type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;

let rec calculator e = match e with
  | X -> raise E
  | INT a -> a
  | ADD (a,b) -> (calculator a) + (calculator b)
  | SUB (a,b) -> (calculator a) - (calculator b)
  | MUL (a,b) -> (calculator a) * (calculator b)
  | DIV (a,b) -> (calculator a) / (calculator b)
  | SIGMA (a,b,c) -> if (calculator a)>(calculator b) then 0 else eval (c,a) + calculator(SIGMA (ADD (a,INT 1),b,c))

and eval (e,v) = match e with
  | X -> calculator v
  | INT a -> a
  | ADD (a,b) -> eval (a,v) + eval (b,v)
  | SUB (a,b) -> eval (a,v) - eval (b,v)
  | MUL (a,b) -> eval (a,v) * eval (b,v)
  | DIV (a,b) -> eval (a,v) / eval (b,v)
  | SIGMA (a,b,c) -> if (eval (a,v))>(eval (b,v)) then 0 else eval (c,INT(eval (a,v))) + calculator(SIGMA (ADD (INT(eval (a,v)),INT 1),INT(eval (b,v)),c));;
