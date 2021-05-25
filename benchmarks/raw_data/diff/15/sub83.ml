type aexp = 
  | Const of int
  | Var of string
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->match aexp with 
|Const _ -> Const 0
|Var y -> if x=y then Const 1 else Const 0
|Power(y, n)-> if x=y then Times [Const n; Power(x, (n-1))] else Const 0
|Times (h::t)->Sum[ Times(diff(h,x)::t) ; Times[h;diff(Times t,x)] ]
|Times []->Const 0
|Sum lsts -> Sum(List.map (fun z->diff(z,x)) lsts);;