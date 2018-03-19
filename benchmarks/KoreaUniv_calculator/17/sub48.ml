(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec cal_SIGMA : exp -> int -> int
= fun e n -> match e with
			| X -> n
			| INT a -> a
			| ADD(a,b) -> ((cal_SIGMA a n) + (cal_SIGMA b n))
			| SUB(a,b) -> ((cal_SIGMA a n) - (cal_SIGMA b n))
			| MUL(a,b) -> ((cal_SIGMA a n) * (cal_SIGMA b n))
			| DIV(a,b) -> ((cal_SIGMA a n) / (cal_SIGMA b n))
			| SIGMA(a, b, f) -> let x = (cal_SIGMA a n) in
								let y = (cal_SIGMA b n) in
								if x>y then raise (Failure "Wrong Range") else
									if x< y then
										(cal_SIGMA f x) + (cal_SIGMA (SIGMA( INT(x+1), b, f)) n)
									else (cal_SIGMA f x);;


let rec calculator : exp -> int
= fun e -> match e with
			| X -> raise (Failure "Error")
			| INT n -> n
			| ADD(a,b) -> (calculator a + calculator b)
			| SUB(a,b) -> (calculator a - calculator b)
			| MUL(a,b) -> (calculator a * calculator b)
			| DIV(a,b) -> (calculator a / calculator b)
			| SIGMA(a, b, f) -> let x = (calculator a) in
								let y = (calculator b) in
								if x > y then raise (Failure "Wrong Range") else
									if x < y then 
										((cal_SIGMA f x) + (calculator (SIGMA(INT (x+1), b, f))))
									else (cal_SIGMA f x);;
