type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, x) ->
  let rec fmap f lst =
    match lst with [] -> [] | x :: xs -> f x :: fmap f xs
  in

  let rec diff' var expr =
    match expr with
    | Sum lst -> Sum (fmap (diff' var) lst)
    | Times lst -> (
        match lst with
        | [] -> Const 0
        | x :: xs ->
            Sum
              [
                Times [ diff' var x; Times xs ];
                Times [ x; Times (fmap (diff' var) xs) ];
              ] )
    | Power (v, e) ->
        if v = var then Times [ Const e; Power (v, e - 1) ] else Const 0
    | Var v -> if v = var then Const 1 else Const 0
    | Const i -> Const 0
  in
  diff' x exp


let _ = diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")
