type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
| Var x -> Const 1
| Power (x, n) -> Times [Const n; Power (x, n-1)]
| Times [a; b] -> Times [a; (diff (b, x))]
| Sum l -> (match l with 
	|hd::tl -> Sum ((diff (hd,x))::[diff(Sum tl, x)])
	|[] -> Const 0)
| _ -> Const 0
