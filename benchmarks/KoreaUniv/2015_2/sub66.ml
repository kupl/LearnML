(*********************)
(* Problem 1: filter *)
(*********************)

let rec filter : ('a -> bool) -> 'a list -> 'a list
=fun pred lst -> match lst with
									| [] -> []
									| x::tail -> if (pred x)
																 then x::(filter pred tail)
															else (filter pred tail)

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
							| [] -> b
							| x::tail -> x::(zipper (b, tail))

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> if n = 0 then (fun (x : int) -> x)
							else (fun (x : int) -> f (iter((n-1), f) x))

(*********************)
(* Problem 4: Diff   *)
(*********************)
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

(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
=fun e -> match e with
				| X -> raise(Failure  "Invalid Syntax")
				| INT a -> a
				| ADD (x,y) -> calculator(x) + calculator(y)
				| SUB (x,y) -> calculator(x) - calculator(y)
				| MUL (x,y) -> calculator(x) * calculator(y)
				| DIV (x,y) -> calculator(x) / calculator(y)
				| SIGMA (x,y,z) -> if calculator(x) <= calculator(y) 
														then result(z,x) + calculator(SIGMA(INT(calculator(x)+1),y,z))
													else 0

and result : (exp * exp) -> int
=fun (e,a) -> match e with
							| ADD(x,y) -> result(x,a) + result(y,a)
							| SUB(x,y) -> result(x,a) - result(y,a)
							| MUL(x,y) -> result(x,a) * result(y,a)
							| DIV(x,y) -> result(x,a) / result(y,a)
							| SIGMA(x,y,z) -> calculator(e)
							| INT a -> a
							| X -> calculator(a)
