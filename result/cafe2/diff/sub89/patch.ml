type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const c -> Const 0
  | Var x -> if x = var then Const 1 else Const 0
  | Power (x, n) ->
      if x != var then Const 0 else Times [ Const n; Power (x, n - 1) ]
  | Times lexp -> (
      match lexp with
      | [] -> Const 1
      | [ hd ] -> diff (hd, var)
      | hd :: tl -> (
          match hd with
          | Const 0 -> Const 0
          | Const 1 -> diff (Times tl, var)
          | Const c -> Times [ Const c; diff (Times tl, var) ]
          | _ ->
              Sum
                [
                  Times (diff (hd, var) :: tl);
                  Times [ hd; diff (Times tl, var) ];
                ] ) )
  | Sum lexp -> (
      match lexp with
      | [] -> Const 0
      | [ hd ] -> diff (hd, var)
      | hd :: tl -> Sum [ diff (hd, var); diff (Sum tl, var) ] )
