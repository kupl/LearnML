type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator_sub : exp -> int -> int
= fun exp i -> match exp with
  | X -> i
  | INT x -> x
  | ADD (x, y) -> (calculator_sub x i) + (calculator_sub y i)
  | SUB (x, y) -> (calculator_sub x i) - (calculator_sub y i)
  | MUL (x, y) -> (calculator_sub x i) * (calculator_sub y i)
  | DIV (x, y) -> (calculator_sub x i) / (calculator_sub y i)
  | SIGMA (x, y, z) -> 
    sigma (calculator_sub x i) (calculator_sub y i) z

and sigma : int -> int -> exp -> int
= fun st ed exp -> if st > ed then 0
else (calculator_sub exp st) + (sigma (st + 1) ed exp);;

let rec calculator : exp -> int
= fun exp -> calculator_sub exp 0;;

(*
calculator (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)));;
*)