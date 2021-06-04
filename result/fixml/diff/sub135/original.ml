type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (e, x) ->
  match e with
  | Const y -> Const 0
  | Var s -> if s = x then Const 1 else Var s
  | Times al -> (
      match al with
      | hd :: tl -> Sum [ Times [ diff (hd, x); Sum tl ]; diff (Sum tl, x) ] )
  | Power (s, n) ->
      if s = x then Times [ Const n; Power (s, n - 1) ] else Power (s, n)
  | Sum m -> (
      match m with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
