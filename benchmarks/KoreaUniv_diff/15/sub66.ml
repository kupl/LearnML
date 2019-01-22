type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
								| Const _ -> Const 0
								| Var a -> if a <> x then Const 0
														else Const 1
								| Power (a,b) -> if a = x 
																		then Times[Const b;Power(x, (b-1))]
																	else Const 0
								| Times (head::tail) -> Sum(Times(diff(head,x)::tail)::[Times(head::[diff(Times(tail),x)])])
								| Times [] -> Const 0
								| Sum (head::tail) -> Sum(diff(head,x)::[diff(Sum(tail),x)])
								| Sum [] -> Const 0
