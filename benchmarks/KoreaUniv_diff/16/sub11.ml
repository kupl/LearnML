
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec map f l =
  	match l with
	| [] ->	[]
	| hd::tl ->	(f hd)::(map f tl)

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
  match exp with
  | Const n ->	Const 0
  | Var str ->	if str = var then Const 1 else Const 0
  | Power (_, 0) ->	Const 0
  | Power (str, n) ->	if str <> var then Const 0 else Times [Const n; Power (str, n - 1)]
  | Times [] ->	Const 0
  | Times (hd::tl) ->	Sum [Times ((diff (hd, var))::tl); Times [hd; diff (Times tl, var)]]
  | Sum [] ->	Const 0
  | Sum l ->	Sum (map (fun exp -> diff (exp, var)) l)