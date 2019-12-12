
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

	let rec diff_sum : aexp list * string -> aexp list
	= fun (aexplist, var) -> match aexplist with
	| [] -> [Const 0]
	| hd::tl -> [diff(hd, var)]@(diff_sum(tl, var))

	and diff_times : aexp list * string -> aexp
	= fun (aexplist, var) -> match aexplist with
	| [] -> Const 0
	| hd::tl -> Sum [Times ([diff(hd, var)]@tl); Times [hd; diff_times(tl, var)]]

  and diff : aexp * string -> aexp
	= fun (exp, var) -> match exp with
	| Const num -> Const 0
	| Var st -> if st=var then Const 1 else Const 0
	| Power (st, num) -> if st=var then Times [Const num; Power (st, num-1)] else Const 0
	| Times aexplist -> diff_times(aexplist@[Const 1], var)
	| Sum aexplist -> Sum (diff_sum(aexplist, var))
