
type aexp = 
	|Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list


let rec diff : aexp* string -> aexp = fun (exp, var) -> 
   match exp with
  |Const a-> Const 0
  |Var b -> if b = var then Const 1 else Const 0
  |Power (s, n) ->
  if s = var then Times [Const n ; Power (s, n - 1 )] else Const 0
  |Times [] -> Const 0
  |Times (h::t) -> 
	Sum [Times ((diff(h, var))::t); Times[h; diff(Times t, var)]]
  |Sum [] -> Const 0
  |Sum (h::t) -> Sum [diff(h,var); diff(Sum t, var)]