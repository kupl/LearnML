type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let diff ((e : aexp), (x : string)) : aexp =
  let rec df ((e : aexp), (x : string)) : aexp =
    match e with
    | Sum hd :: tl -> Sum [ df (hd, x); df (Sum tl, x) ]
    | Sum _ -> Const 0
    | Times hd :: tl ->
        if hd = Var x then Times (tl @ [ Const 1 ])
        else Times [ hd; df (Times tl, x) ]
    | Times _ -> Times [ Const 0 ]
    | Power (s, i) -> if s = x then Times [ Const i; Var s ] else Const 0
    | Const i -> Const 0
    | Var s -> if s = x then Const 1 else Const 0
  in
  df (e, x)