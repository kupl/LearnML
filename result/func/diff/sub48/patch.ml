type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const a -> Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Power (a, b) ->
      if a = x then Times [ Const b; Power (a, b - 1) ] else Const 0
  | Times [] -> Const 0
  | Times __s62 :: __s63 ->
      Sum
        [
          Times [ diff (__s62, x); Times __s63 ];
          Times [ diff (Times __s63, x); __s62 ];
        ]
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
