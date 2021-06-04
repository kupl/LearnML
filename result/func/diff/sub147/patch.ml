type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const a -> Const 0
  | Var a -> if x = a then Const 1 else Const 0
  | Power (a, n) ->
      if a = x then
        if n >= 2 then Times [ Const n; Power (x, n - 1) ]
        else if n = 1 then Const 1
        else Const 0
      else Const 0
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | [ e1; e2 ] ->
          Sum [ Times [ diff (e1, x); e2 ]; Times [ e1; diff (e2, x) ] ]
      | hd :: tl ->
          Sum
            [
              Times [ diff (hd, x); Times tl ]; Times [ hd; diff (Times tl, x) ];
            ] )
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | [ e1; e2 ] -> Sum [ diff (e1, x); diff (e2, x) ]
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
