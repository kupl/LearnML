(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) ->
        match e with
        | Const _ -> Const 0
        | Var y -> if x = y then Const 1 else Const 0
        | Power (y, n) -> if x = y then Times [Const n; Power(y, n-1)] else Const 0
        | Times l ->
                (match l with
                | [] -> Const 0
                | hd::tl -> Sum [Times ((diff (hd, x))::tl); Times [hd; (diff (Times tl, x))]])
        | Sum l ->
                (match l with
                | [] -> Const 0
                | hd::tl -> Sum [diff (hd, x); diff (Sum tl, x)])
