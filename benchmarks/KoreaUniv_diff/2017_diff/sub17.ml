(* problem 4 *)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp = fun (e, x) ->
  match e with
  | Const _ -> Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Power (b, n) -> if b <> x then Const 0 else
    (match n with
     | 0 -> Const 0
     | c -> Times [Const c; Power (b, c - 1)]
    )
  | Times fx -> 
    (match fx with
      | [] -> Const 0
      | hd::[] -> diff (hd, x)
      | hd::tl ->  Sum [Times [diff (hd, x); Times tl]; Times [hd; diff (Times tl, x)]]
    )
  | Sum el ->
    (match el with
     | [] -> Const 0
     | hd::[] -> diff (hd, x)
     | hd::tl -> Sum [diff (hd, x); diff (Sum tl, x)]

    )