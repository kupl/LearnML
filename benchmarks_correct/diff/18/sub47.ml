type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  match exp with
    |Const a -> Const 0
    |Var t -> if t = x then Const 1 else Const 0
    |Power (t, a) -> if t = x then Times [Const a; Power (t, a-1)] else Const 0
    |Times l ->
    begin
      match l with
        |[] -> Const 0
        |hd::tl -> Sum [Times ([diff (hd,x)]@tl) ; Times (hd::[diff (Times tl, x)])]
    end
    |Sum l ->
      match l with
        |[] -> Const 0
        |hd::tl -> Sum [diff (hd,x); diff (Sum tl,x)];;

diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;