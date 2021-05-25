type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
	fun (exp, x) ->
	match exp with
	| Const n -> Const 0
	| Var a -> if(a=x) then Const 1 else Const 0
	| Power (var, num) -> if(var=x&&num=1) then Const 1 else if(var=x&&num=2) then Times (Const num::Var var::[]) else if(var=x) then Times (Const num::Power (var, num-1)::[]) else Const 0
	| Times e -> (match e with
	  |hd::tl -> if(tl=[]) then diff (hd,x) else Sum(Times (diff (hd, x)::tl)::Times (hd::diff (Times tl, x)::[])::[])
	  |[] -> Times e)
 	| Sum e -> match e with
 	  |hd::tl -> if (tl=[]) then diff (hd, x) else Sum(diff (hd, x)::diff (Sum tl, x)::[])
 	  |[] -> Sum e;;
  	  
