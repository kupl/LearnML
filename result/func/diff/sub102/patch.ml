type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec map (f : 'c * 'b -> 'a) ((l : 'c list), (var : string)) : 'a list =
  match l with [] -> [] | hd :: tl -> f (hd, var) :: map f (tl, var)


let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const n -> Const 0
  | Var a -> if var = a then Const 1 else Const 0
  | Power (a, n) ->
      if var = a then
        match n with
        | 0 -> Const 0
        | 1 -> Const 1
        | 2 -> Times [ Const 2; Var a ]
        | _ -> Times [ Const n; Power (a, n - 1) ]
      else Const 0
  | Sum l -> Sum (map diff (l, var))
  | Times l -> (
      match l with
      | [] -> Const 0
      | hd :: tl -> (
          match hd with
          | _ ->
              Sum
                [
                  Times [ diff (hd, var); Times tl ];
                  Times [ hd; diff (Times tl, var) ];
                ] ) )
