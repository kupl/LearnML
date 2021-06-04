type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const a -> Const 0
  | Var x -> if x = var then Const 1 else Const 0
  | Power (x, n) ->
      if x != var then Const 0 else Times [ Const n; Power (x, n - 1) ]
  | Times l -> (
      match l with
      | [] -> Const 0
      | h :: t ->
          Sum [ Times (diff (h, var) :: t); Times [ h; diff (Times t, var) ] ] )
  | Sum m -> (
      match m with
      | [] -> Const 0
      | h :: t -> Sum [ diff (h, var); diff (Sum t, var) ] )
