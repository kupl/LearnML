type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (var : string)) : aexp =
  match exp with
  | Const a -> Const 0
  | Var a -> if a = var then Const 1 else Const 0
  | Power (a, b) -> (
      match b with
      | 0 -> Const 0
      | 1 -> if a = var then Const 1 else Const 0
      | 2 -> if a = var then Times [ Const 2; Var a ] else Const 0
      | _ -> if a = var then Times [ Const b; Power (a, b - 1) ] else Const 0 )
  | Times li -> (
      match li with
      | [] -> Times []
      | hd :: tl ->
          if tl = [] then diff (hd, var)
          else if hd = Const 0 then Const 0
          else
            let h : aexp = diff (hd, var) in

            let t : aexp = diff (Times tl, var) in
            if h = Const 0 then Times [ hd; t ]
            else if t = Const 0 then Times (h :: tl)
            else Sum [ Times (h :: tl); Times [ hd; t ] ] )
  | Sum li -> (
      match li with
      | [] -> Sum []
      | hd :: tl ->
          if tl = [] then diff (hd, var)
          else Sum [ diff (hd, var); diff (Sum tl, var) ] )
