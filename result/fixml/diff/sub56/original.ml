type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff (aexp, x) =
  match aexp with
  | Const a -> Const 0
  | Var s -> if s = x then Const 1 else Const 0
  | Power (s, a) ->
      if a = 1 then Const 1 else Times [ Const a; Power (s, a - 1) ]
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | h :: t ->
          if t = [] then
            Sum
              [ Times [ diff (h, x); Const 1 ]; Times [ h; diff (Times t, x) ] ]
          else
            Sum
              [ Times [ diff (h, x); Times t ]; Times [ h; diff (Times t, x) ] ]
      )
  | Sum lst2 -> (
      match lst2 with
      | [] -> Const 0
      | h :: t -> Sum [ diff (h, x); diff (Sum t, x) ] )
