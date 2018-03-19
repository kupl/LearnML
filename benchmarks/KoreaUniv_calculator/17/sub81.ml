
(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> 
	match e with
	| X -> calculator (INT 0)
	| INT n -> n
	| ADD (INT n1, INT n2) -> (n1+n2)
	| ADD (e1,e2) -> ((calculator e1)+(calculator e2))
	| SUB (INT n1, INT n2) -> (n1-n2)
	| SUB (e1, e2) -> ((calculator e1)-(calculator e2))
	| MUL (INT n1, INT n2) -> (n1*n2)
	| MUL (e1, e2) -> ((calculator e1)*(calculator e2))
	| DIV (INT n1, INT n2) -> (n1/n2)
	| DIV (e1, e2) -> ((calculator e1)/(calculator e2))
	| SIGMA (e1, e2, e3) -> let rec sigma : exp-> exp -> int
													=fun e3 e1 -> 
													match e3 with
													| X -> calculator e1
													| INT n -> n
													| ADD (a,b) -> (sigma a e1) + (sigma b e1)
  												| SUB (a,b) -> (sigma a e1) - (sigma b e1)
  												| MUL (a,b) -> (sigma a e1) * (sigma b e1)
												  | DIV (a,b) -> (sigma a e1) / (sigma b e1)
												  | SIGMA (a,b,c) -> calculator (SIGMA (a,b,c))
													in if ((calculator e1)=(calculator e2)) then (sigma e3 e1) 
													else (calculator (SIGMA (ADD (e1, INT 1), e2, e3)))+(sigma e3 e1)
	
