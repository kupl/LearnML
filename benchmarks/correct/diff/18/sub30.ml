type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  match exp with
    |Const n -> Const 0
    |Var y -> if x = y then Const 1 else Const 0
    |Power(str,n) -> 
      if str = x then 
        match n with 
          |0 -> Const 0
          |1 -> Const 1
          |_ -> Times[Const n; Power(str,n-1)]
      else Const 0
    |Times lst -> 
      (match lst with
        |[] -> Const 0
        |head :: tail -> 
            Sum [Times(diff(head,x)::tail); Times[head; diff(Times tail,x)]]
      )
    |Sum lst -> 
      match lst with
        |[] -> Const 0
        |head :: tail ->
          Sum[diff(head,x) ; diff(Sum tail,x)];;
    