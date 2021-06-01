
let rec element f s = 
match s with
| [] -> []
| hd::tl -> (f hd)::(element f tl) 

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
| Var v -> if x=v then Const 1 else Const 0
| Power (p,n) ->
  if (x=p) then (if not (n=1) then Times [Const n; Power(p,n-1)] else Const 1) else Const 0
| Times [] -> Const 0
| Times (hd::tl) -> if tl = [] then diff (hd,x)
else Sum [(if diff (hd,x) = Const 0 then Const 0 else Times (diff(hd,x)::tl)); Times [hd; diff (Times tl,x)]]
|Sum lst -> Sum (element (fun l -> diff(l,x)) lst)
