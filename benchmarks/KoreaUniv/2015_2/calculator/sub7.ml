type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
=fun e ->
	match e with
	| X -> raise (Failure "not applicable")
	| SIGMA(e1, e2, e3) ->
		let rec evaluateExpInSig : exp -> int -> int
				= fun expWithX xValue ->
				match expWithX with
				| X -> xValue
				| INT (i) -> i
				| ADD (e1, e2) -> (evaluateExpInSig e1 xValue) + (evaluateExpInSig e2 xValue)
				| SUB (e1, e2) -> (evaluateExpInSig e1 xValue) - (evaluateExpInSig e2 xValue)
				| MUL (e1, e2) -> (evaluateExpInSig e1 xValue) * (evaluateExpInSig e2 xValue)
				| DIV (e1, e2) -> (evaluateExpInSig e1 xValue) / (evaluateExpInSig e2 xValue)
			(*  | DIV (e1, e2) -> (evaluateExpInSig(e1, xValue)) / (evaluateExpInSig(e2, xValue)) 
				 이렇게 하면 오류남... ㅠㅠㅠㅠㅠㅠㅠㅠㅠㅠ 왜인지 질문해서 확실하게 알자*)
				| SIGMA (e'1, e'2, e'3) -> raise (Failure "nested sigma is not applicable for a expression with only one variable \"X\"")
				
			in let rec calcSum : int-> int-> exp -> int
						= fun start en expWithX ->
							if start > en then 0
							else (evaluateExpInSig expWithX start) + (calcSum (start+1) en expWithX)
					in (calcSum (evaluateExpInSig e1 0) (evaluateExpInSig e2 0) e3)
								                    (*second parameter 0 is meaningless here*)
	| INT (i) -> i
	| ADD (e1, e2) -> calculator(e1) + calculator(e2)
	| SUB (e1, e2) -> calculator(e1) - calculator(e2)
	| MUL (e1, e2) -> calculator(e1) * calculator(e2)
	| DIV (e1, e2) -> calculator(e1) / calculator(e2)
;;

(* test
diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;
diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "y");;
diff(Sum [ Sum [Power ("x", 4); Times [Const 2; Var "x"]; Const 1];
	  Sum [ Times [ Const 2; Power ("x", 3) ]; Const 1];
	  Sum [Power ("x", 2)]], "x");;
calculator (SIGMA(INT 9, INT 10, SUB(MUL(X, X), INT 1))) ;;
*)