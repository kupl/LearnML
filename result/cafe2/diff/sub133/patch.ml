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
    | Times __s82 -> (
        match __s82 with
        | [] -> Const 0
        | [ __s83 ] -> df (__s83, x)
        | __s84 :: __s85 -> (
            match e with
            | Const 0 -> Const 0
            | _ ->
                Sum
                  [
                    Times [ __s84; df (Times __s85, x) ];
                    Times [ df (__s84, x); Times __s85 ];
                  ] ) )
    | Times _ -> Times [ Const 0 ]
    | Power (s, i) ->
        if s = x then
          if i = 1 then Const 1 else Times [ Const i; Power (s, i - 1) ]
        else Const 0
    | Const i -> Const 0
    | Var s -> if s = x then Const 1 else Const 0
  in
  df (e, x)