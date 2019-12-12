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
=fun (aexp,x) ->	
	match aexp with
	|Const n -> Const 0
	|Var y -> if y = x then Const 1 else Const 0
	|Power(z, n) -> if z = x then	
				(if n = 0 then Const 0
				else if n = 1 then Const 1
				else Times[Const n; Power(z, n-1)])
			else Const 0
	|Times l1 -> 
		(match l1 with 
		|[] -> Const 0
		|h::[] -> diff (h, x)
		|h::t ->
			(match h with
			|Const n -> Times[Const n; diff(Times t, x)]
			|_ -> Sum[ Times [diff(h,x); Times t]; Times [h; diff(Times t, x)]  ] 
			)
		)
	|Sum l1 -> 	
		(match l1 with	
		|[] -> Const 0
		|h::t -> Sum[diff(h,x);diff(Sum t, x)]
		)
