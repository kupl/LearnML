type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const a -> Const 0
  | Power (a, b) ->
      if a = x then Times [ Const b; Power (a, b - 1) ] else Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Sum lst ->
      let rec loop (lst : aexp list) : aexp list =
        match lst with [] -> [] | hd :: tl -> [ diff (hd, x) ] @ loop tl
      in
      Sum (loop lst)
  | Times lst ->
      let rec loop (lst : aexp list) : aexp list =
        match lst with
        | [] -> []
        | hd :: tl ->
            [
              Times [ diff (hd, x); Times tl ]; Times [ hd; diff (Times tl, x) ];
            ]
      in
      Sum (loop lst)
