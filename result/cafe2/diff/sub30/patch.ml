type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

exception InvalidArgument

let rec diff ((aexp : aexp), (s : string)) : aexp =
  match aexp with
  | Const n -> Const 0
  | Var x -> if s = x then Const 1 else Const 0
  | Power (x, n) ->
      if x != s then Const 0 else Times [ Const n; Power (x, n - 1) ]
  | Times aexpl ->
      if List.length aexpl = 0 then raise InvalidArgument
      else if List.length aexpl = 1 then diff (List.hd aexpl, s)
      else
        Sum
          [
            Times (diff (List.hd aexpl, s) :: List.tl aexpl);
            Times [ List.hd aexpl; diff (Times (List.tl aexpl), s) ];
          ]
  | Sum aexpl ->
      if List.length aexpl = 0 then raise InvalidArgument
      else if List.length aexpl = 1 then diff (List.hd aexpl, s)
      else Sum [ diff (List.hd aexpl, s); diff (Sum (List.tl aexpl), s) ]
