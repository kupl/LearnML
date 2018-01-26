(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list	

let rec diff : aexp * string -> aexp
 = fun (e, x) -> 
	match e with
		| Const n -> (Const 0)
    		| Times t -> (match t with
              			| hd::tl -> (Sum ([Times ([diff (hd, x)] @ tl)] @ [ Times ([hd] @ [diff (Times tl, x)])]))
				| [] -> (Const 0))
    		| Sum s -> (match s with
            			| hd::tl -> (Sum ([diff (hd, x)] @ [diff (Sum tl, x)]))
				| [] -> (Const 0))
		| Var var -> if (compare x var) = 0 then (Const 1) else (Const 0)
		| Power (var, exp) -> if (compare x var) = 0 then (Times ([Const exp] @ [Power (var,exp-1)])) else (Const 0)
