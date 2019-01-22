
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (aexp,x) -> match aexp with
  | Const c -> Const c 
  | Var v -> (match v with
    | x -> Times [Const 0;Var x])
  | Power (p,a) -> (match p,a with
    | x, a-> Times [Const a; Power (x,a-1)])
  | Times [] -> Const 1
  | Times (hd::tl) -> Times [diff(hd,x);diff(Times tl,x)]
  | Sum [] -> Const 0
  | Sum (hd::tl) -> Sum [diff(hd,x);diff(Sum tl,x)]
