type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const a -> Const 0
  | Var a -> Var a
  | Power (a, b) -> if b = 1 then Var a else Times [ Const b; Power (a, b - 1) ]
  | Times a -> (
      match a with
      | hd :: tl -> (
          match hd with
          | Const t ->
              Times
                [ Const t; Sum [ Times [ Const 1 ]; Times [ Var x; Const 0 ] ] ]
          | Var t -> Times [ Var t; Const 0 ] ) )
  | Sum a -> (
      match a with
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ]
      | _ -> Const 0 )
