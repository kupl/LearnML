type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec __s1 ((__s2 : aexp), (__s3 : string)) : aexp =
  match __s2 with
  | Const __s61 -> Const 0
  | Var __s62 -> if __s62 = __s3 then Const 1 else Const 0
  | Power (__s63, __s64) ->
      if __s63 = __s3 then
        match __s64 with
        | 0 -> Const 0
        | 1 -> Const 1
        | 2 -> Times [ Const 2; Var __s3 ]
        | _ -> Times [ Const __s64; Power (__s3, __s64 - 1) ]
      else Const 0
  | Times __s65 :: __s66 ->
      if __s66 = [] then __s1 (__s65, __s3)
      else
        Sum
          [
            Times ([ __s1 (__s65, __s3) ] @ __s66);
            Times ([ __s65 ] @ [ __s1 (Times __s66, __s3) ]);
          ]
  | Sum __s67 :: __s68 ->
      if __s68 = [] then __s1 (__s67, __s3)
      else Sum ([ __s1 (__s67, __s3) ] @ [ __s1 (Sum __s68, __s3) ])
  | Times [] -> Const 0
  | Sum [] -> Const 0


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


let rec diff ((exp : aexp), (var : string)) : aexp = __s1 (exp, var)
