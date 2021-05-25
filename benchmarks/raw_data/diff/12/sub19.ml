(* Ex 2 *)
type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

exception ILLEGAL_EXPRESSION of string 
let rec diff((aexp:aexp), (str:string)) = match aexp with 
				    | Const(_) -> Const(0)
				    | Var(x) -> if x = str 
						then Const(1)
						else Const(0)
				    | Power(base, expnt) -> if base = str 
							    then Times( [Const(expnt) ; Power(base, expnt - 1)] )
							    else Const(0)
				    | Times([]) -> raise(ILLEGAL_EXPRESSION "The argument of Times is illegal")
				    | Times(hd::[]) -> diff(hd, str)
				    | Times(hd::tl) -> Sum([Times( [diff(hd, str)]@tl )]@[Times(hd::[diff(Times(tl), str)])])
				    | Sum([]) -> raise(ILLEGAL_EXPRESSION "The argument of Sum is illega")
				    | Sum(hd::[]) -> diff(hd, str)
				    | Sum(hd::tl) -> Sum( [diff(hd, str)] @ [diff(Sum(tl), str)] )