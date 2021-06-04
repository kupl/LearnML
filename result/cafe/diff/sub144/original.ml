type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let diff ((e : aexp), (x : string)) : aexp =
  let rec func (aexp : aexp) (x : string) : aexp =
    match aexp with
    | Const _ -> Const 0
    | Var y -> if x = y then Const 1 else Const 0
    | Power (y, n) ->
        if x = y then Times [ Const n; Power (y, n - 1) ] else Const 0
    | Times lst -> (
        match lst with
        | [] -> raise Failure "fail"
        | [ t ] -> raise Failure "fail"
        | [ t1; t2 ] -> Sum [ Times [ func t1 x; t2 ]; Times [ t1; func t2 x ] ]
        | hd :: tl ->
            Sum
              [ Times [ func hd x; Times tl ]; Times [ hd; func (Times tl) x ] ]
        )
    | Sum lst -> (
        match lst with
        | [] -> raise Failure "fail"
        | [ s ] -> raise Failure "fail"
        | [ s1; s2 ] -> Sum [ func s1 x; func s2 x ]
        | hd :: tl -> Sum [ func hd x; func (Sum tl) x ] )
  in
  func e x
