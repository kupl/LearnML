
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list


  let diff : aexp * string -> aexp
  = fun (exp, var) -> exp;;
	
	let rec diff2 : aexp * string -> aexp
	= fun (exp, var) ->
		(match exp with
		 Sum s ->
				(match s with
				[]->Const 0
				|hd::[] -> diff2(hd, var)
				|hd::tl -> Sum[diff2 (hd, var); diff2 (Sum tl, var)])
				
		| Times s ->
				(match s with
				[]->Const 1
				|hd::[] -> (diff2(hd,var))
				|hd::tl ->  
						(match hd with
						Const n -> Times [Const n; diff2(Times tl, var)]
						|_ -> (Sum[Times [hd; diff2(Times tl, var)] ; Times (diff2 (hd,var) :: tl)])))
		| Power (s, n) -> 
				(if s=var then ( match n with
					0 -> Const 0
				|1 -> Const 1
				|_ -> Times [Const n; Power(s,n-1)])
				else Const 0)
		| Const n -> Const 0
		| Var x -> 
				if(x=var) then Sum[Const 1; Times[Var x;Const 0]]
				else Const 0);;
	let diff : aexp * string -> aexp
	=fun (exp, var) -> diff2 (exp, var);;