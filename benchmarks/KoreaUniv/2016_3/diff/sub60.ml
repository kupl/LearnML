
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list		

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
		|Const n -> Const 0
		| Var t -> if t=var then Const 1 else Const 0
		|Power (t,n) -> if t=var then Times [Const n; Power (t,n-1)] else Const 0
		|Times lst ->Times ( timediff (lst,var))
		|Sum lst -> Sum (sumdiff (lst,var))
	
	and timediff : aexp list*string -> aexp list
	= fun (lst, var) -> match lst with
		|[]-> [Const 1]
		|hd::tl -> (match hd with
			|Const n->Const n
			|Var t->if t=var then Const 1 else Const 0
			|Power (t,n) -> if t=var then Times [Const n; Power (t,n-1)] else Const 0
			|Times l2 ->Times ( timediff (l2,var))
			|Sum l2 -> Sum (sumdiff (l2,var)))::(timediff (tl,var))
	
	and sumdiff : aexp list*string -> aexp list
	= fun (lst, var) -> match lst with
		|[]->[Const 0]
		|hd::tl -> (match hd with 
			|Const n->Const 0
			|Var t-> if t=var then Const 1 else Const 0
			|Power (t,n) -> if t=var then Times [Const n; Power (t,n-1)] else Const 0
			|Times l2 -> Times (timediff (l2,var))
			|Sum l2 -> Sum (sumdiff (l2,var)))::(sumdiff (tl,var))