type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list;;

let rec diff : aexp * string -> aexp
= fun (aexp,x) ->
	match aexp with
	Const n -> Const 0
| Var y -> if y=x then Const 1 else Const 0
| Power (s,n) ->
	(if s=x then
		(match n with
		0 -> Const 0
	| 1 -> Const 1
	| 2 -> Times [Const 2; Var s]
	| _ -> Times [Const n; Power (s,n-1)])
	else Const 0)
| Times l ->
	(match l with
		[] -> Const 1
	| h::[] -> (diff(h,x))
	| h::t ->
		(match h with
			Const 1 -> (diff(Times t,x))
		| Const n -> (Times [Const n; diff(Times t,x)])
		| _ -> (Sum [Times (diff(h,x)::t); Times [h; diff(Times t,x)]])))
| Sum l ->
	(match l with
		[] -> Const 0
	| h::[] -> (diff(h,x))
	| h::t ->  (Sum [diff(h,x); diff(Sum t,x)]));;
