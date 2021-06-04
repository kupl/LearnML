type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const a -> Const 0
  | Var v -> if v = var then Const 1 else Var v
  | Power (v, a) ->
      if v = var then
        match a with
        | 2 -> Times [ Const 2; Var v ]
        | 1 -> Const 1
        | 0 -> Const 0
        | _ -> Times [ Const a; Power (v, a - 1) ]
      else Power (v, a)
  | Times l -> (
      match l with
      | h :: t ->
          if t = [] then diff (h, var)
          else
            Sum [ Times (diff (h, var) :: t); Times [ h; diff (Times t, var) ] ]
      )
  | Sum l ->
      let rec subsum (l : aexp list) : aexp list =
        match l with
        | [] -> []
        | h :: t -> (
            match h with
            | Const 0 -> subsum t
            | Times [ Const 0 ] -> subsum t
            | _ -> diff (h, var) :: subsum t )
      in
      Sum (subsum l)
