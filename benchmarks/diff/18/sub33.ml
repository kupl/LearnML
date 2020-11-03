type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  match exp with
  | Const n -> Const 0
  | Var s -> if s = x then Const 1 else Const 0
  | Power(s, n) -> if s = x then Times[Const n; Power(s, n-1)] else Const 0
  | Times lst ->
  begin
    match lst with
    | [] -> Const 0
    | h::t -> Sum[Times([diff(h, x)]@t); Times[h; diff(Times t, x)]]
  end
  | Sum lst ->
  begin
    match lst with
    | [] -> Const 0
    | h::t -> Sum[diff(h, x); diff(Sum t, x)]
  end;;
