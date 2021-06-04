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
      if s = x then
        if n = 0 then Const 0
        else if n = 1 then Const 1
        else Times [ Const n; Power (x, n - 1) ]
      else if x = s && n != 0 then Times [ Const n; Power (x, n - 1) ]
      else Const 0
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
