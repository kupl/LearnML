type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const i -> Const 0
  | Var k -> if k != x then Const 0 else Const 1
  | Power (k, i) ->
      if k != x then Const 0 else Times [ Const i; Power (k, i - 1) ]
  | Times l -> (
      match l with
      | [] -> Const 0
      | h :: t ->
          Sum [ Times (diff (h, x) :: t); Times [ h; diff (Times t, x) ] ] )
  | Sum l -> (
      match l with
      | [] -> Const 0
      | h :: t -> Sum [ diff (h, x); diff (Sum t, x) ] )
