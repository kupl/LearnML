exception EMPTYLIST

type aexp =
  | Const of int
  | Var of string
  | Sum of aexp list
  | Times of aexp list
  | Power of (string * int)

let rec diff (aexp, str) =
  match aexp with
  | Const a -> Const 0
  | Var b -> if str = b then Const 1 else Const 0
  | Power (pstr, i) ->
      if str = pstr then Times [ Const i; Power (pstr, i - 1) ] else Const 0
  | Times alst -> (
      match alst with
      | [] -> Const 0
      | [ h ] -> diff (h, str)
      | h :: t ->
          Sum [ Times (diff (h, str) :: t); Times [ h; diff (Times t, str) ] ] )
  | Sum [] -> Const 0
  | [ Sum h ] -> diff (h, str)
  | Sum h :: t -> Sum [ diff (h, str); diff (Sum t, str) ]
