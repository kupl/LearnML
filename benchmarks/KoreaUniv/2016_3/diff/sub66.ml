
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->  (* TODO *)
	match exp with
	| Const(a) -> Const 0
	| Var a when a=var -> Const 1
	| Power(a,b) when var=a -> Times[Const b; Power(a, b-1)]
	| Times l -> (match l with
						|	hd::tl when diff(hd,var)=Const 0 -> (match hd with
																									| Const a -> Times[hd; diff(Times tl, var)]
																									| _-> Times[Const 0; diff(Times tl, var)])
						| hd::tl -> Times[diff(hd, var); diff(Times tl, var)] 
						| [] -> Const 1)
	| Sum l -> (match l with
						| hd::tl -> Sum[diff(hd, var); diff(Sum tl, var)]
						| [] -> Const 0)
	| _->Const 0			