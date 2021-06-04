type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec map f (l, var) =
  match l with [] -> [] | hd :: tl -> f (hd, var) :: map f (tl, var)


let rec diff : aexp * string -> aexp =
 fun (exp, var) ->
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
      | [] -> Const 1
      | hd :: tl -> (
          match hd with
          | Power (a, n) ->
              if var = a then
                if tl = [] then diff (Power (a, n), var)
                else Times [ diff (Power (a, n), var); Times tl ]
              else if tl = [] then hd
              else Times [ hd; diff (Times tl, var) ]
          | Var a ->
              if var = a then
                if tl = [] then Const 1 else Times [ Const 1; Times tl ]
              else Times [ hd; diff (Times tl, var) ]
          | _ -> if tl = [] then Const 1 else Times [ hd; diff (Times tl, var) ]
          ) )
