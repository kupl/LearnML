(*problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
	
let rec diff : aexp * string -> aexp
= fun (e,x) -> 
	match e with
	| Const n -> Const 0
	| Var x1 -> if x1=x then Const 1 else Const 0
	| Power (x1,n) -> if x1=x then if n=1 then Const 1 else Times [Const n; Power (x,n-1)]
										else Const 0
	| Sum l -> (match l with
						 | [] -> Const 0
						 | hd::tl -> Sum [diff (hd,x); diff ((Sum tl),x)]) 
  | Times l -> match l with
						 | [] -> Const 0
						 | hd::tl -> Sum[(Times ((diff (hd,x))::tl)); (Times [hd;(diff ((Times tl),x))])]						
