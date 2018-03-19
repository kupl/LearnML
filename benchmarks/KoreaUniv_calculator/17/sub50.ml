(*problem 5*)
type exp = X
	|INT of int 
	|ADD of exp * exp
	|SUB of exp * exp
	|MUL of exp * exp
	|DIV of exp * exp
	|SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> match e with
			|X -> raise(Failure "type error")
			|INT a -> a
			|ADD (a,b) -> calculator a + calculator b
			|SUB (a,b) -> calculator a - calculator b
			|MUL (a,b) -> calculator a * calculator b
			|DIV (a,b) -> calculator a / calculator b
			|SIGMA (st,ls,x) -> let a = calculator st in 
								let b = calculator ls in
								if a = b then
									let rec sigc = fun idx ex -> match ex with
									|X -> sigc idx idx
									|INT n -> n
									|ADD (j,k) -> (sigc idx j) + (sigc idx k)
									|SUB (j,k) -> (sigc idx j) - (sigc idx k)
									|MUL (j,k) -> (sigc idx j) * (sigc idx k)
									|DIV (j,k) -> (sigc idx j) / (sigc idx k)
									|SIGMA (s,l,pol) -> calculator e
									in sigc ls x
							else (calculator (SIGMA (st, st, x)) + calculator (SIGMA (ADD(st,INT 1),ls,x)))
