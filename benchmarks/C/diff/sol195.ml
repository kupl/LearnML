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
|Const n -> Const 0
|Var v -> if v=x then Const 1 else Const 0
|Power (s, i) ->
if s=x then Times[Const i; Power(s, i-1)] else Const 0
|Times [] -> Const 0
|Times (hd::tl) -> Sum [Times [diff (hd,x); Times tl]; Times [diff (Times tl,x); hd]]
|Sum [] -> Const 0
|Sum (hd::tl) -> Sum [diff (hd, x); diff (Sum tl, x)]
