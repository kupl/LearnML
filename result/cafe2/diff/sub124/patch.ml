type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((e : aexp), (x : string)) : aexp =
  let rec timediff ((tlst : aexp list), (x : string)) : aexp list =
    match tlst with
    | [] -> []
    | hd :: tl ->
        if tl = [] then [ diff (hd, x) ]
        else
          [ Times ([ diff (hd, x) ] @ tl) ]
          @ [ Times ([ hd ] @ [ Sum (timediff (tl, x)) ]) ]
  in

  let rec sumdiff ((slst : aexp list), (x : string)) : aexp list =
    match slst with [] -> [] | hd :: tl -> diff (hd, x) :: sumdiff (tl, x)
  in

  let rec differ ((e : aexp), (x : string)) : aexp =
    match e with
    | Const c -> Const 0
    | Var v -> if v = x then Const 1 else Const 0
    | Power (v, c) ->
        if v = x then Times [ Const c; Power (v, c - 1) ] else Const 0
    | Sum lst -> Sum (sumdiff (lst, x))
    | Times lst -> Sum (timediff (lst, x))
  in
  differ (e, x)
