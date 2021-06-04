type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec p1_help ((aexp : aexp list), (var : string)) : aexp list =
  match aexp with [] -> [] | hd :: tl -> diff (hd, var) :: p1_help (tl, var)


and diff ((aexp : aexp), (var : string)) : aexp =
  match aexp with
  | Const n -> Const 0
  | Var y -> if y = var then Const 1 else Const 0
  | Power (y, n) ->
      if y = var then Times [ Const n; Power (y, n - 1) ] else Const 0
  | Times __s65 :: __s66 ->
      Sum
        [
          Times (diff (__s65, var) :: __s66);
          Times [ __s65; diff (Times __s66, var) ];
        ]
  | Times aexp1 -> (
      match aexp1 with
      | [] -> Const 0
      | hd :: tl ->
          Sum [ Times (diff (hd, var) :: tl); Times (hd :: p1_help (tl, var)) ]
      )
  | Sum aexp1 -> Sum (p1_help (aexp1, var))
