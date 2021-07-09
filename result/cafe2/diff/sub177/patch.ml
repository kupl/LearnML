type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  let rec fmap (f : aexp -> aexp) (lst : aexp list) : aexp list =
    match lst with [] -> [] | x :: xs -> f x :: fmap f xs
  in

  let rec diff' (var : string) (expr : aexp) : aexp =
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

  match exp with
  | Const __s58 -> Const 0
  | Var __s59 -> if __s59 = x then Const 1 else Const 0
  | Power (__s60, __s61) ->
      if __s60 = x then Times [ Const __s61; Power (x, __s61 - 1) ] else Const 0
  | Times [] -> Const 0
  | Times __s62 :: __s63 ->
      Sum
        [
          Times (diff (__s62, x) :: __s63);
          Times [ __s62; diff (Times __s63, x) ];
        ]
  | Sum [] -> Const 0
  | Sum __s64 :: __s65 -> Sum [ diff (__s64, x); diff (Sum __s65, x) ]


let (_ : aexp) =
  diff (Sum [ Power ("x", 2); Times [ Const 2; Var "x" ]; Const 1 ], "x")
