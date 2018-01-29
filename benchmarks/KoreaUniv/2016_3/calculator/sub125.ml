(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec f: int -> exp -> int  
  = fun i exp ->
    match exp with
      | X -> i
			| INT a -> a
			| ADD(a, b) -> f i a + f i b
      | SUB(a, b) -> f i a - f i b
      | MUL(a, b) -> f i a * f i b
      | DIV(a, b) -> f i a / f i b
      | SIGMA(a, b, c) -> f i a + f i b + f i c;;

  let rec calculator : exp -> int
  = fun exp -> 
    match exp with
      | INT(a) -> a
      | ADD(exp1, exp2) 
        -> calculator(exp1) + calculator(exp2)
      | SUB(exp1, exp2) 
        -> calculator(exp1) - calculator(exp2)
      | MUL(exp1, exp2) 
        -> calculator(exp1) * calculator(exp2)
      | DIV(exp1, exp2) 
        -> calculator(exp1) / calculator(exp2)
      | SIGMA(exp1, exp2, exp3) 
        -> if calculator(exp1) > calculator(exp2) then 0
           else if calculator(exp1) = calculator(exp2) 
              then f (calculator (exp1)) exp3
           else f (calculator(exp1)) exp3
              + calculator(SIGMA(INT(calculator(exp1)+1), exp2, exp3));;