type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const n -> Const 0
  | Var x -> if x = var then Const 1 else Var x
  | Power (x, n) ->
      if x = var then Times [ Const n; Power (x, n - 1) ] else Power (x, n)
  | Times l -> (
      match l with
      | [] -> raise Failure "Times error"
      | h :: t ->
          Sum [ Times (diff (h, var) :: t); Times [ h; diff (Times t, var) ] ] )
  | Sum l -> (
      match l with
      | [] -> raise Failure "Sum error"
      | h :: t -> Sum [ diff (h, var); diff (Sum t, var) ] )
