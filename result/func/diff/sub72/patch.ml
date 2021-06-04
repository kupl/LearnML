type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const y -> Const 0
  | Var t -> if t = x then Const 1 else Const 0
  | Power (s, i) ->
      if s = x then Times ([ Const i ] @ [ Power (s, i - 1) ]) else Const 0
  | Times k -> (
      match k with
      | [] -> Const 0
      | hd :: tl ->
          Sum
            ( [ Times ([ diff (hd, x) ] @ tl) ]
            @ [ Times ([ hd ] @ [ diff (Times tl, x) ]) ] ) )
  | Sum k -> (
      match k with
      | [] -> Const 0
      | h :: t -> Sum ([ diff (h, x) ] @ [ Sum [ diff (Sum t, x) ] ]) )
