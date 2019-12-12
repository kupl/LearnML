type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
match aexp with
|Const n -> Const 0
|Var y -> if y=x then Const 1 else Const 0
|Power(y, n) -> if y=x then (
                    if n=0 then Const 0
                    else if n=1 then Const 1
                    else Times[Const n; Power(y, n-1)])
                else Const 0
|Times l -> 
  (match l with
  |[] -> Const 0
  |hd::[] -> diff(hd,x)
  |hd::tl -> 
    (match hd with
      |Const n -> Times[Const n; diff(Times tl, x)]
      |_ -> Sum[ Times [diff(hd,x); Times tl]; Times [hd; diff(Times tl, x)]  ] ))
|Sum l -> 
  (match l with
  |[]-> Const 0
  |hd::tl -> Sum[diff(hd, x); diff(Sum tl, x)]);;
