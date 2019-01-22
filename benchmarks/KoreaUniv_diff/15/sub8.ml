type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
| Const n-> Const 0
| Var v-> if v=x then Const 1 else Const 0 
| Power (v,n) -> if v=x then Times [Const n;Power(v,n-1)] else Const 0
| Times l->(match l with 
	| []-> Const 0
	| h::t -> Sum[Times (diff (h,x)::t); Times(h::diff(Times t,x)::[])]) 
| Sum l->let rec f l ll= (match l with
	| [] -> Sum ll
	| h::t-> let ll=ll@((diff(h,x))::[]) in f t ll) in f l []