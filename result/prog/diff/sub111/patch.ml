type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const a -> Const 0
  | Var str -> if str = var then Const 1 else Const 0
  | Power (str, a) ->
      if str = var then Times [ Const a; Power (str, a - 1) ] else Const 0
  | Times lst -> Sum (difftime (lst, var))
  | Sum lst -> Sum (diffsum (lst, var))


and diffsum ((lst : aexp list), (var : string)) : aexp list =
  match lst with [] -> [] | hd :: tl -> [ diff (hd, var) ] @ diffsum (tl, var)


and difftime ((lst : aexp list), (var : string)) : aexp list =
  match lst with
  | [] -> []
  | hd :: tl ->
      if tl = [] then [ Times ([ diff2 (hd, var) ] @ tl) ]
      else
        [ Times ([ diff2 (hd, var) ] @ tl) ]
        @ [ Times ([ hd ] @ [ Sum (difftime (tl, var)) ]) ]


and diff2 ((hd : aexp), (var : string)) : aexp =
  match hd with
  | Const a -> Const 0
  | Var str -> if str = var then Const 1 else Const 0
  | Power (str, a) ->
      if str = var then Times [ Const a; Power (str, a - 1) ] else Const 0
  | Times lst -> diff (Times lst, var)
  | Sum lst -> diff (Sum lst, var)
