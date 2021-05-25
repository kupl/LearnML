type aexp = | Const of int | Var of string | Power of string * int | Times of aexp list | Sum of aexp list 


let rec diff : aexp * string -> aexp
= fun (aexp,x) -> 
match aexp with
|Const n -> Const 0
|Var v-> 
if x = v then Const 1
 else Const 0
|Power (v, aexp') ->
 if x=v then Times [Const aexp'; Power (v, aexp'-1)]
 else Const 0
|Times lst ->
(match lst with
|[] -> Const 0
|hd::tl -> Sum [Times (diff (hd, x)::tl); Times [hd; diff (Times tl, x)]])
|Sum lst ->
(match lst with
|[] -> Const 0
|hd::tl -> Sum [diff(hd,x); diff(Sum tl, x)])
