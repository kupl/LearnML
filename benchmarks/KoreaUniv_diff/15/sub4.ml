
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
match aexp with
|Const a -> Const 0
|Var "x" -> Const 1
|Power ("a", n) -> if n = 1 then Const 1 else Times [Const n; Power ("a", n-1)]
|Times [a;b] -> Times [diff (aexp,x); diff(aexp,x)]
|Sum [a;b] -> Sum [diff (aexp,x); diff(aexp,x)]
