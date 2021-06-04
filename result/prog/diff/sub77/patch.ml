type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const i -> Const 0
  | Var s -> diff (Power (s, 1), var)
  | Power (s, i) ->
      if s = var then Times [ Const i; Power (s, i - 1) ] else Const 0
  | Sum lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, var); diff (Sum tl, var) ] )
  | Times lst -> (
      match lst with
      | [] -> Const 0
      | hd :: tl ->
          if diff (hd, var) = Const 0 then Times [ hd; diff (Times tl, var) ]
          else
            Sum
              [
                Times ([ diff (hd, var) ] @ tl);
                Times [ hd; diff (Times tl, var) ];
              ] )
