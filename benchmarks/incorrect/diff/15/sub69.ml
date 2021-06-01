type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
| Const a -> Const 0
| Var b -> if b=x then Const 1 else Var b
| Power(b,a) -> if b=x then Times [Const a; Power (x,a-1)] else Power(b,a)
| Times l -> (match l with
[] -> Const 0
| hd::tl -> (match hd with
Var b -> if b=x then Times ((diff (hd,x))::tl)
	else Times (hd::[diff (Times tl,x)])
| Power (b,a) -> if b=x then Times ((diff (hd,x))::tl)
	else Times (hd::[diff (Times tl,x)])
|_ -> Times (hd::[diff (Times tl,x)])))
| Sum l -> (match l with
[] -> Const 0
| hd::tl -> Sum ((diff (hd,x))::[diff (Sum tl,x)]));;
