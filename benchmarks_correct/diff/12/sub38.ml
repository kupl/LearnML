type aexp = 
  Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff (exp, d) =
  match exp with
  Const i -> Const 0
  | Var str -> 
      if str = d then (Const 1)
      else (Const 0)
  | Power (str, i) ->
      if i == 0 then Const 0
      else if (i == 1 & str = d) then Const 1 
      else if str = d then (Times ((Const i)::[Power (str, (i - 1))]))
      else Const 0
  | Sum sList ->
      (
        match sList with
        [] -> Const 0
        | [hd] -> diff (hd, d)
        | (hd::tl) -> Sum [(diff (hd, d));(diff ((Sum tl), d))]
      )
  | Times tList ->
      (
        match tList with
        [] -> (Const 0)
        | [hd] -> diff (hd, d)
        | (hd::tl) -> Sum [(Times ((diff (hd, d))::tl)) ; (Times [hd;(diff((Times tl), d))])]
      )
