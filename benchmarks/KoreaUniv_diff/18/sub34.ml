type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
and var = string;;

let rec diff : aexp * string -> aexp
= fun (exp, x) -> match exp with
  | Const x -> x
  | Var 
  | Power 
  | Times 
  | Sum 