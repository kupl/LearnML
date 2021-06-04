type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl ->
          if tl = [] then diff (hd, x)
          else Sum ([ diff (hd, x) ] @ [ diff (Sum tl, x) ]) )
  | Const n -> Const 0
  | Var str -> if str = x then Const 1 else Const 0
  | Power (str, n) ->
      if str = x then Times [ Const n; Power (x, n - 1) ] else Const 0
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl -> Times ([ hd ] @ [ diff (Sum tl, x) ]) )
