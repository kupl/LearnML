type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  match e with
  | Const a -> Const 0
  | Var a -> if a = x then Const 1 else Var a
  | Power (a, b) ->
      if a = x && b = 1 then Const 1
      else if a = x then Times [ Const b; Power (a, b - 1) ]
      else Power (a, b)
  | Times a -> (
      match a with
      | [ n1; n2 ] ->
          if n1 = Const 0 || n2 = Const 0 then Const 0
          else if n1 = Var x then n2
          else if n2 = Var x then n1
          else Times [ n1; diff (n2, x) ] )
  | Sum a ->
      let rec help_diff (e1 : aexp list) : aexp list =
        match e1 with [] -> [] | hd :: tl -> diff (hd, x) :: help_diff tl
      in
      Sum (help_diff a)
