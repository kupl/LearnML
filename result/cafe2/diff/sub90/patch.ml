type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (var : string)) : aexp =
  match aexp with
  | Const cst1 -> Const 0
  | Var var1 -> if var1 = var then Const 1 else Const 0
  | Power (str, int1) ->
      if str != var then Const 0
      else Times [ Const int1; Power (str, int1 - 1) ]
  | Sum sum_lst -> (
      match sum_lst with
      | [] -> Const 0
      | [ hd ] -> diff (hd, var)
      | hd :: tl -> Sum [ diff (hd, var); diff (Sum tl, var) ] )
  | Times tm_lst -> (
      match tm_lst with
      | [] -> Const 1
      | [ hd ] -> diff (hd, var)
      | hd :: tl ->
          Sum
            [
              Times [ diff (hd, var); Times tl ];
              Times [ hd; diff (Times tl, var) ];
            ] )
