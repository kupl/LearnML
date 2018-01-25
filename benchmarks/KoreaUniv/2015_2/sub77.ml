
let rec filter p l =
	match l with
	[]->[]
	|hd::tail->if (p hd) = true then hd::(filter p tail)
		else (filter p tail)
let rec zipper ( list1 , list2 ) =
	match list1 with
		[]->list2
		|hd_of_l1::tail_of_l1 -> match list2 with
									[]->list1
									|hd_of_l2::tail_of_l2 -> hd_of_l1::(hd_of_l2::(zipper (tail_of_l1 , tail_of_l2)))
	
let rec iter ((n:int),f)(*: int *(int->int)->(int->int)*) =
	match n with
	0-> fun (x:int) -> x 
	|_-> fun (x:int) -> f (iter(n-1,f) x)



type aexp =
	|Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

let rec diff (aexp, x) = 
	match aexp with	
		Const int1 -> Const 0 
		|Var x -> Const 1
		|Power (var,int1) -> Times [Const int1;Power (var,int1-1)]
		|Times lst ->( match lst with
					[]-> Const 1
					|hd::tail -> if (diff ( hd , x )) = (Const 0) then Times (hd::[(diff (Times tail, x))]) 
								else if (diff (hd,x)) = (Const 1) then Times (Const 1::Const 1::tail) 
								else Times (Const 1::(diff (hd,x))::tail)
				)
		|Sum lst -> match lst with
			[]-> Const 0
			|hd::tail->Sum ((diff(hd,x))::[(diff ( (Sum tail),x))])


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







