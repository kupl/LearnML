 type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp ;;
  
exception PROBLEM;;
(*let temp = DIV(X,X);;*)
(*SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1))*)

let rec calculator : exp -> int
= fun exp ->
	match exp with
  | X-> 0
	| INT a -> a
	| ADD (a,b) -> if a<>(X) && b<>(X) then (calculator a) + (calculator b) else raise PROBLEM
	| SUB (a,b) -> if a<>(X) && b<>(X) then (calculator a) - (calculator b) else raise PROBLEM
	| MUL (a,b) -> if a<>(X) && b<>(X) then (calculator a) + (calculator b) else raise PROBLEM
	| DIV (a,b) ->  if a<>(X) && b<>(X) then (calculator a) / (calculator b)  else raise PROBLEM
	| SIGMA (n,m,f) -> sigma_sum n m f (*start from integer n to integer m*)
  
  and sigma_sum: exp->exp->exp->int
  = fun n m f->
    if (calculator n) <= (calculator m)
      then (sub_calculator n f) + (sigma_sum  (INT ((calculator n) + 1)) m f)
      else 0

  and sub_calculator: exp->exp->int
  = fun x exp ->
    match exp with 
    | X -> calculator x
    | INT a -> a
  	| ADD (a,b) ->  (sub_calculator x a) + (sub_calculator x b) 
  	| SUB (a,b) ->  (sub_calculator x a) - (sub_calculator x b) 
  	| MUL (a,b) -> (sub_calculator x a ) * (sub_calculator x b) 
  	| DIV (a,b) ->  (sub_calculator x a) / (sub_calculator x b)
  	| SIGMA(n,m,f) -> sigma_sum x m f 
;;
(*calculator (ADD(X,X));;*)
(*calculator (SIGMA(INT 1, INT 10, SIGMA(X, INT 10, SIGMA(X, INT 10, X))) );;*)
(*calculator (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)));;*)
