type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let diff ((e : aexp), (x : string)) : aexp =
  let rec helper ((e : aexp), (x : string)) : aexp =
    match e with
    | Const a -> Const 0
    | Var v -> if v = x then Const 1 else Const 0
    | Power (s, i) ->
        if s = x then Times [ Const i; Power (s, i - 1) ] else Const 0
    | Sum __s76 -> (
        match __s76 with
        | [] -> Sum []
        | [ __s77 ] -> helper (__s77, x)
        | __s78 :: __s79 -> (
            match e with
            | Const _ -> helper (Sum __s76, x)
            | _ ->
                Sum
                  (List.append [ helper (__s78, x) ] [ helper (Sum __s79, x) ])
            ) )
    | Times __s82 -> (
        match __s82 with
        | [] -> Const 0
        | [ __s83 ] -> helper (__s83, x)
        | __s84 :: __s85 -> (
            match __s84 with
            | Const 0 -> Const 0
            | _ ->
                Sum
                  [
                    Times [ __s84; helper (Times __s85, x) ];
                    Times [ helper (__s84, x); Times __s85 ];
                  ] ) )
  in
  helper (e, x)