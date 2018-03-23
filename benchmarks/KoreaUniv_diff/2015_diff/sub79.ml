type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
match aexp with
Const i-> Const 0
|Var a-> if a="x" then Const 1 else Const 0
|Power (a,i)-> Times[Const i; Power (a,i-1)]
|Times (h::t)-> Times (h::[diff (Sum t,x)])
|Sum (h::t)-> Sum ((diff (h,x))::[diff (Sum t,x)])
|_-> Const 0

