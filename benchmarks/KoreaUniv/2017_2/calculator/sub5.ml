
(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> let funb env = match env with | [] -> failwith "failure" | hd::tl -> hd
	   in let rec funa e env = match e with | X -> funb env
					        | INT a -> a
					        | ADD(a,b) -> (funa a env) + (funa b env) 
					        | SUB(a,b) -> (funa a env) - (funa b env) 
					        | MUL(a,b) -> (funa a env) * (funa b env)
					        | DIV(a,b) -> if (funa b env) != 0 then (funa a env) / (funa b env) else failwith "Divided by Zero"
					        | SIGMA(a,b,c) -> if (funa a env) = (funa b env) then (funa c ((funa b env)::env))
								else ((funa (SIGMA(b,b,c)) env) + (funa (SIGMA(a,SUB(b,INT 1),c)) env))
		in funa e [];;