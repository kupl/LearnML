type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

exception InvalidArgument

let rec diff (aexp, s) =
  match (aexp, s) with
  | Sum [], s -> raise InvalidArgument
  | Times [], s -> raise InvalidArgument
  | Const i, s -> Const 0
  | Var s1, s -> if s1 = s then Const 1 else Const 0
  | Power (s1, a), s ->
      if s1 = s then Times [ Const a; Power (s1, a - 1) ]
      else Times [ Const a; diff (Var s1, s); Power (s1, a - 1) ]
  | Sum [ a; b ], s -> Sum [ diff (a, s); diff (b, s) ]
  | Sum a :: sl, s -> Sum [ diff (a, s); diff (Sum sl, s) ]
  | Times [ a; b ], s ->
      Sum [ Times [ diff (a, s); b ]; Times [ a; diff (b, s) ] ]
  | Times a :: sl, s ->
      Sum [ Times (diff (a, s) :: sl); Times [ a; diff (Times sl, s) ] ]
