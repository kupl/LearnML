type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let rec calculator2
=fun e ->  match e with
						|X -> 1 (*wrong*)
						|INT n -> n
						|ADD(a,b) -> (calculator2 a)+(calculator2 b)
						|SUB(a,b) -> (calculator2 a)-(calculator2 b)
						|MUL(a,b) -> (calculator2 a)*(calculator2 b)
						|DIV(a,b) -> (calculator2 a)/(calculator2 b)
						|SIGMA(a,b,c) -> if (calculator2 a) > (calculator2 b) then 0 else (calculator2 c) + calculator2( SIGMA (INT ((calculator2 a)+1),b,c));;

let calculator : exp -> int
= fun e -> calculator2(e);;
