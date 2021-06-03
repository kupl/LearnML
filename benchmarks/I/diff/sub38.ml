type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
| Const(i) -> Const(0)
| Var(s) -> if s=x then Const(1) else Const(0)
| Power(s, i) -> (
	if s=x then (
	match i with
	| 0 -> Const(0)
	| 1 -> Const(1)
	| 2 -> Times[Const(2); Var(s)]
	| _ -> Times[Const(i); Power("x", i-1)]) else Const(0)
	)
| Times(l) -> (
	match l with
	| [] -> Const(0)
	| hd::tl-> Sum([Times([diff(hd,x)] @ tl)] @ [Times([hd] @ [diff(Times(tl),x)])])
	)
| Sum(l) -> (
	match l with
	| [] -> Const(0)
	| hd::tl -> Sum([diff(hd,x)] @ [diff(Sum(tl),x)])
	)
