type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let calculator : exp -> int
=fun e ->  let rec eval e v b n = (match e with
	| X -> if b = 0 then raise (Failure "Failed") else n
	| INT n -> n
	| ADD (e1,e2) -> (eval e1 v b n) + (eval e2 v b n)
	| SUB (e1,e2) -> (eval e1 v b n) - (eval e2 v b n)
	| MUL (e1,e2) -> (eval e1 v b n) * (eval e2 v b n)
	| DIV (e1,e2) -> if (eval e2 v b n) = 0 then raise (Failure "Failed") else (eval e1 v b n) / (eval e2 v b n)
	| SIGMA (e1,e2,e3) ->	 let e4 = eval e1 v b n in
				 let e5 = eval e2 v b n in
				 let n = e4 in
				 let b = 1 in	 (if e4>e5 then v else
                        	 let v = v + (eval e3 v b n) in	eval (SIGMA (INT (e4+1),INT e5,e3)) v b n) )  in eval e 0 0 0

