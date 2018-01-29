
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list 

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
		match (exp, var) with
		| (Const n, v) -> Const 0
		| (Var x, v) -> if x = v then Const 1 else Const 0
		| (Power (v1, n), v) -> if v1 = v then Times [Const n; Power(v1, n-1)] else Const 0
		| (Times l, v) -> (match l with
			| (Const n :: tl) -> Times ((Const (n*(iter v l)))::tl)
			| _ -> Const 0)
		| (Sum l, v) -> (match l with
				| [] -> Const 0
		  	| (hd::tl) -> Sum ((diff (hd, v))::((diff ((Sum tl), v))::[])))
	
	and count x e =
		match e with
		| Const n -> 0
		| Var v -> if x = v then 1 else 0
		| Power (v,n) -> if v = x then n else 0
		| Times l -> iter x l
		| Sum l -> 0
	and iter x l =
 		match l with
		| [] -> 0
		| hd::tl -> (count x hd) + (iter x tl)
