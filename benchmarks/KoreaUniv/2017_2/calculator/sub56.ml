(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> let rec f e x = match e with
	| X -> x 
	| (INT n) -> n 
	| ADD (n,m) -> (f n x) + (f m x) 
	| SUB (n,m) -> (f n x) - (f m x) 
	| MUL (n,m) -> (f n x) * (f m x)
	| DIV (n,m) -> if (f m x) = 0 then raise (Failure "zero divide error") else (f n x)/(f m x)
	| SIGMA (n,m,k) -> if ((f n x) <= (f m x)) then  (f k (f n x)) + (f (SIGMA (ADD(INT 1, n),m,k)) x)  else 0
	in f e 0
