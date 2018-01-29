
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec map f (l,var) =
  	match l with
  	| [] -> []
  	| hd::tl -> (f (hd,var))::(map f (tl,var))

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
 	match exp with
 	| Const n -> Const 0
 	| Var x -> if x <> var then Const 0 else Const 1
 	| Power(x, n) -> if x <> var then Const 0 else Times [Const n; Power(x, (n-1))]
 	| Times lst ->
 		begin
	 		match lst with
	 		| [] -> Const 0
	 		| hd::tl -> Sum [Times ((diff (hd,var))::tl);Times [hd;diff ((Times tl),var)]]
 		end
 	| Sum lst -> Sum (map diff (lst,var))