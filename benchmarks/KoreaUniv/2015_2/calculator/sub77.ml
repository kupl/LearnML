type exp = X
		|INT of int
		|ADD of exp * exp
		|SUB of exp * exp
		|MUL of exp * exp
		|DIV of exp * exp
		|SIGMA of exp * exp * exp

exception X_NOT_MATCHED 

let rec calculator exp =
		match exp with 
			X-> raise X_NOT_MATCHED 
			|INT int1 -> int1
			|ADD (exp1,exp2) -> calculator exp1 + calculator exp2 
			|SUB (exp1,exp2) -> calculator exp1 - calculator exp2
			|MUL (exp1,exp2) -> calculator exp1 * calculator exp2
			|DIV (exp1,exp2) -> calculator exp1 / calculator exp2
			|SIGMA (int1,int2,exp)-> if (calculator int1) = (calculator int2) then match exp with
													X -> calculator int1
													|INT k -> k
													|ADD (exp1,exp2) -> calculator (SIGMA (int1,int2,exp1)) + calculator (SIGMA (int1,int2,exp2))
													|SUB (exp1,exp2) -> calculator (SIGMA (int1,int2,exp1)) - calculator (SIGMA (int1,int2,exp2))
													|MUL (exp1,exp2) -> calculator (SIGMA (int1,int2,exp1)) * calculator (SIGMA (int1,int2,exp2))
													|DIV (exp1,exp2) -> calculator (SIGMA (int1,int2,exp1)) / calculator (SIGMA (int1,int2,exp2))
													|_-> calculator exp (*SIGMA in SIGMA*)
									else calculator (SIGMA (int1,int1,exp)) + calculator (SIGMA (INT ((calculator int1)+1),int2,exp))







