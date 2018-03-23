
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
	| Const n -> Const 0
	| Var str -> if str = var then Const 1 else Var str 
	| Power (str, i) -> if str = var then Times[Const i; Power(str, i-1)] else Power(str, i)
	| Times lst -> begin
	match lst with
	| [] -> Const 0 
	| hd::tl -> Times [hd ; diff(tl, var)]
end
	| Sum lst -> begin
	match lst with
	| [] -> Const 0
	| hd::tl -> Sum [diff(hd, var) ; (diff(tl, var))]
end