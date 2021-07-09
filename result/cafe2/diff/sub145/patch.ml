type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const c -> Const 0
  | Var v -> if v != x then Const 0 else Const 1
  | Power (str, po) ->
      if str != x then Const 0 else Times [ Const po; Power (str, po - 1) ]
  | Times l -> (
      match l with
      | [] -> Const 0
      | hd :: tl ->
          let inner_hd : aexp =
            match tl with hd2 :: tl2 -> hd2 | [] -> Const 0
          in
          Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ] )
  | Sum l -> (
      match l with
      | [] -> Const 0
      | hd :: tl ->
          let inner_hd : aexp =
            match tl with hd2 :: tl2 -> hd2 | [] -> Const 0
          in
          Sum [ diff (hd, x); diff (Sum tl, x) ] )
