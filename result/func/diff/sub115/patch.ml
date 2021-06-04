type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let diff ((e : aexp), (x : string)) : aexp =
  let rec f ((e : aexp), (x : string)) : aexp =
    match e with
    | Const n -> Const 0
    | Var n -> if n = x then Const 1 else Const 0
    | Power (n, i) ->
        if n = x then Times [ Const i; Power (n, i - 1) ] else Const 0
    | Times lst -> (
        match lst with
        | [] -> Const 0
        | h :: t -> (
            match h with
            | _ -> Sum [ Times [ h; f (Times t, x) ]; Times (f (h, x) :: t) ] )
        )
    | Sum lst -> (
        match lst with
        | [] -> Const 0
        | h :: t -> Sum [ f (h, x); f (Sum t, x) ] )
  in
  f (e, x)
