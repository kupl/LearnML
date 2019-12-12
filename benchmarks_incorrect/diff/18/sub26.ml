type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> (*aexp*)
match exp with
  | Const n -> Const 0 
  | Var v -> if v = x then Const 1 else Const 0
  | Power(v, n) -> if v = x then Times [Const n; Power(v, n-1)] else Const 0
    |Times l -> (match l with
        | [] -> Const 0
        | h::t -> Sum[Times[diff(h, x)] ; Times[h ; diff(Times t, x)]] )
    |Sum l -> match l with
        | [] -> Const 0
        | h::t -> Sum[diff(h, x); diff(Times t, x)];;
        

diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;
