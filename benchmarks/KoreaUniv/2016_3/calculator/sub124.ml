
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
	  let rec sigmaCalc: exp * int -> int 
	  = fun (exp, a) -> match (exp, a) with 
		| X, a -> a
		| INT x,a -> x
		| ADD (x, y),a -> sigmaCalc (x,a) + sigmaCalc (y,a)
		| SUB (x, y),a -> sigmaCalc (x,a) - sigmaCalc (y,a)
		| MUL (x, y),a -> sigmaCalc (x,a) * sigmaCalc (y,a)
		| DIV (x, y),a -> sigmaCalc (x,a) / sigmaCalc (y,a)
		| SIGMA (INT (x), INT (y), z),a -> if x > y then raise (Failure "Invalid input") else if x = y then sigmaCalc (z,a) else sigmaCalc (z,a) + sigmaCalc(SIGMA(INT(x+1), INT (y), z), (x+1)) 
		| SIGMA (x, y, z),a -> raise (Failure "Invalid input")

	  let rec calculator : exp -> int
	  = fun exp -> match exp with 
		| X -> raise (Failure "Invalid input")
		| INT x -> x
		| ADD (x, y) -> calculator x + calculator y
		| SUB (x, y) -> calculator x - calculator y
		| MUL (x, y) -> calculator x * calculator y
		| DIV (x, y) -> calculator x / calculator y
		| SIGMA (INT (x), INT (y),z) -> if x > y then raise (Failure "Invalid input") else sigmaCalc (exp, x)
		| SIGMA (x, y, z) -> raise (Failure "The first two varibles of Sigma exp have to be integers")
