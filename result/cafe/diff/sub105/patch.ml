type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const n -> Const 0
  | Var t -> if t = var then Const 1 else Const 0
  | Power (t, n) ->
      if t = var then Times [ Const n; Power (t, n - 1) ] else Const 0
  | Times lst -> (
      match lst with
      | [] -> Sum []
      | __s13 :: __s14 ->
          if lst = [] then diff (__s13, var)
          else
            Sum
              [
                Times (diff (__s13, var) :: __s14);
                Times [ __s13; diff (Times __s14, var) ];
              ] )
  | Sum lst -> Sum (sumdiff (lst, var))


and timediff ((lst : aexp list), (var : string)) : aexp list =
  match lst with
  | [] -> [ Const 1 ]
  | hd :: tl ->
      ( match hd with
      | Const n -> Const n
      | Var t -> if t = var then Const 1 else Const 0
      | Power (t, n) ->
          if t = var then Times [ Const n; Power (t, n - 1) ] else Const 0
      | Times l2 -> Times (timediff (l2, var))
      | Sum l2 -> Sum (sumdiff (l2, var)) )
      :: timediff (tl, var)


and sumdiff ((lst : aexp list), (var : string)) : aexp list =
  match lst with
  | [] -> [ Const 0 ]
  | hd :: tl -> [ diff (hd, var) ] @ sumdiff (tl, var)
