type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec hasVar ((exp : aexp list), (var : string)) : bool =
  match exp with
  | hd :: tl ->
      ( match hd with
      | Const x -> false
      | Var x -> if x = var then true else false
      | Power (x, y) ->
          if x = var then if y != 0 then true else false else false
      | Times l -> hasVar (l, var)
      | Sum l -> hasVar (l, var) )
      || hasVar (tl, var)
  | [] -> false


let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Power (x, y) -> (
      match y with
      | 0 -> Const 0
      | 1 -> if x = var then Const 1 else Const 0
      | 2 -> if x = var then Times [ Const 2; Var x ] else Const 0
      | _ -> if x = var then Times [ Const y; Power (x, y - 1) ] else Const 0 )
  | Const x -> Const 0
  | Var x -> if x = var then Const 1 else Const 0
  | Sum l -> (
      match l with
      | [] -> Const 0
      | [ x ] -> diff (x, var)
      | hd :: tl -> Sum [ diff (hd, var); diff (Sum tl, var) ] )
  | Times l -> (
      let flag : bool = hasVar (l, var) in

      match l with
      | [] -> Const 0
      | [ x ] ->
          if flag = true then
            if diff (x, var) = Const 0 then x else diff (x, var)
          else Const 0
      | hd :: tl -> (
          match tl with
          | [] -> diff (hd, var)
          | [ __s65 ] ->
              Sum
                [
                  Times [ diff (hd, var); __s65 ];
                  Times [ hd; diff (__s65, var) ];
                ]
          | _ ->
              Sum
                [
                  Times [ diff (hd, var); Times tl ];
                  Times [ hd; diff (Times tl, var) ];
                ] ) )
