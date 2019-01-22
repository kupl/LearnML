type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list
  |Times l ->
	(match l with
		|[] -> Const 0
		|hd::tl -> Sum[Times((diff (hd,x))::tl);Times((hd)::diff(Times tl,x)::[])]
	)
  |Sum l ->
	(match l with
		|[] -> Const 0
		|hd::tl -> Sum[diff(hd,x);diff(Sum tl,x)]
	)
