type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
  | Const(b) -> Const 0
  | Var(a) -> if x<>a then Const 0 else Const 1
  | Power(a, b) ->
    if x<>a then Const 0
    else if b=1 then diff (Var a, x)
    else Times[Const b; Power(a, (b-1))]
  | Times(lst) -> (match lst with
    | [] -> Const 0 
    | hd::tl -> Times[hd; diff(Sum(tl), x)])
  | Sum(lst) -> (match lst with
    | [] -> Const 0
    | hd::tl -> Sum[diff(hd, x); diff(Sum(tl), x)])
