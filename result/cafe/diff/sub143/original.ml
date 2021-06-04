type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const i -> Times []
  | Var s -> Const 1
  | Power (s, i) ->
      if s = x then Times [ Const i; Power (s, i - 1) ] else Power (s, i)
  | Times l -> (
      match l with
      | [] -> Times []
      | hd :: tl -> (
          match hd with Const n -> hd | _ -> Times ([ diff (hd, x) ] @ tl) ) )
  | Sum l -> (
      match l with
      | [] -> Const 0
      | hd :: tl -> Sum ([ diff (hd, x) ] @ [ diff (Sum tl, x) ]) )
