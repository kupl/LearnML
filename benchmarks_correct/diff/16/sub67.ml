
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
		match exp with
		| Const n -> Const 0
		| Var str -> if str = var then Const 1
					else Const 0
		| Power (str, n) -> 
			if str = var then Times [Const n; Power(str, n - 1)] 
			else Const 0
		| Times lst -> 
			begin
			match lst with 
			| [] -> Const 0
			| hd::tl -> Sum [Times ((diff (hd, var))::tl);Times[hd;diff((Times tl),var)]]
			end
		| Sum lst ->
			match lst with
			| [] -> Const 0
			| hd::tl -> Sum [diff (hd, var); diff((Sum tl), var)]