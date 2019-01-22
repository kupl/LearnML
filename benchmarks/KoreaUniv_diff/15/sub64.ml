type aexp = 
| Const of int 
| Var of string 
| Power of string * int 
| Times of aexp list
| Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> 
  match aexp with
  | Const _ -> Const 0
  | Var (a) -> 
    if a = x then Const 1
    else Const 0
  | Power (a, n) ->
    (match (a, n) with 
     | ("x", 0) -> Const 0
     | ("x", 1) -> Const n
     | ("x", _) -> Times [Const n; Power(a, n-1)]
     | _ -> Const 0)
  | Times (lst) ->  
    if (List.length lst) = 0 then Const 0
    else if (List.length lst) = 1 then diff (List.hd lst, x)
    else Times [List.hd lst; diff (Sum (List.tl lst), x)]
  | Sum (sum) ->
    if (List.length sum) = 0 then Const 0
    else if (List.length sum) = 1 then diff(List.hd sum, x) 
    else Sum [ diff(List.hd sum, x) ; diff (Sum (List.tl sum),x)]
