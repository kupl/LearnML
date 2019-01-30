type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  match exp with
    | Const (a) -> Const 0
    | Var (v) -> if v == x then Const 1 else Var v 
    | Power(v,n) -> if v == x  then Times[Const n; Power(v,n-1)] else Power(v,n)
    | Times (str)-> Sum[Times[(diff(List.hd(str), x))::List.tl(str)];diff (List.tl(str), x)]
    | Sum (str) ->  Sum[diff(List.hd(str),x);diff(List.tl(str),x)];;