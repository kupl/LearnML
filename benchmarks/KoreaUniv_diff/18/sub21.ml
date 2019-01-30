type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  match exp with
    | Const a -> if a = 1 then Const 0 else Const a
    | Var a -> if a = x then Const 1 else Var a
    | Power (n, m) -> if n = x then Times [Const m; Power(n, m - 1)] else Power (n, m)
    | Times [lst] -> 
      match [lst] with 
        | n :: m -> Sum [Times [diff(n, x); diff(m, x)]]
    | Sum [lst] -> 
      match [lst] with 
        | n :: m -> Sum [diff(n, x); diff(m, x)] ;;