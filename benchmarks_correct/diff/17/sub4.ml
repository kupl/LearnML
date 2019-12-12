(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
  fun (e,x) -> match e with
  | Const n -> Const 0
  | Var n -> if n = x then Const 1 else Const 0
  | Power (e1, e2) ->
    if e1 = x then Times[Const e2; Power(e1, e2 - 1)] else Const 0
  | Times lst1 ->
    (match lst1 with
    [] -> Const 0
    | hd :: tl ->
      Sum([Times([diff(hd, x)]@tl); Times([hd;diff(Times tl, x)])]))
  | Sum lst2 ->
    (match lst2 with
    [] -> Const 0
    | hd :: tl -> Sum[diff(hd, x); diff((Sum tl), x)]);;
