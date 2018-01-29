
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
			match exp with
			|Const a -> Const 0
			|Var x -> if x = var then Const 1 else Var x
			|Power (s, i) ->
				if s = var then Times[Const i; Power (s, i-1)] else Power(s, i)
			|Times l -> begin
				match l with
				|[] -> Const 1
				|[e]-> diff (e, var)
				|hd::tl -> Sum( Times (diff(hd, var)::tl) :: Times(hd::diff(Times(tl), var)::[])::[]) end
			|Sum l ->
				match l with
				|[] -> Const 0
				|[e]-> diff (e, var)
				|hd::tl -> Sum(diff(hd, var)::diff(Sum(tl), var)::[])