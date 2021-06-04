type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec processtimelist ((l : aexp), (var : string)) : aexp =
  match l with
  | Const i -> Const i
  | Var str -> if str = var then Const 0 else Var str
  | Power (str, i) ->
      if str = var then
        match i with
        | 2 -> Times [ Const 2; Var str ]
        | 1 -> Const 1
        | 0 -> Const 0
        | _ -> Times [ Const i; Power (str, i - 1) ]
      else Power (str, i)
  | Times hd :: tl ->
      if findlist (hd :: tl, var) > 0 then
        Sum
          ( [ Times (processtimelist (hd, var) :: tl) ]
          @ [ Times ([ hd ] @ [ processtimelist (Times tl, var) ]) ] )
      else Const 1
  | _ -> makediff (l, var)


and processsumlist ((l : aexp list), (var : string)) : aexp list =
  match l with
  | [] -> []
  | hd :: tl -> (
      match hd with
      | Var str ->
          if str = var then Const 1 :: processsumlist (tl, var)
          else Const 0 :: processsumlist (tl, var)
      | _ -> makediff (hd, var) :: processsumlist (tl, var) )


and findlist ((l : aexp list), (str : string)) : int =
  match l with
  | [] -> 0
  | hd :: tl -> (
      match hd with
      | Const i -> findlist (tl, str)
      | Var str2 ->
          if str = str2 then 1 + findlist (tl, str) else findlist (tl, str)
      | Power (str2, i) ->
          if str = str2 then i + findlist (tl, str) else findlist (tl, str)
      | Times l -> findlist (l, str) + findlist (tl, str)
      | Sum l -> findlist (l, str) + findlist (tl, str) )


and makediff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const i -> Const 0
  | Var str -> if str = var then Const 1 else Const 0
  | Power (str, i) ->
      if str = var then
        match i with
        | 2 -> Times [ Const 2; Var str ]
        | 1 -> Const 1
        | 0 -> Const 0
        | _ -> Times [ Const i; Power (str, i - 1) ]
      else Const 0
  | Times hd :: tl ->
      if findlist (hd :: tl, var) > 0 then
        Sum
          ( [ Times (processtimelist (hd, var) :: tl) ]
          @ [ Times ([ hd ] @ [ processtimelist (Times tl, var) ]) ] )
      else Const 1
  | Times [] -> Const 0
  | Sum hd :: tl ->
      if findlist (hd :: tl, var) > 0 then
        Sum ([ makediff (hd, var) ] @ [ makediff (Sum tl, var) ])
      else Const 1
  | Sum [] -> Const 1


let rec diff ((exp : aexp), (var : string)) : aexp = makediff (exp, var)
