type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const i -> Const 0
  | Var s -> if s = x then Const 1 else Var s
  | Power (s, i) ->
      if s = x then Times [ Const i; Power (s, i - 1) ] else Power (s, i)
  | Times hd :: hd' :: tl -> Times [ hd; diff (hd', x) ]
  | Sum li ->
      let rec diffForExpList (expList : aexp list) : aexp list =
        match expList with
        | [] -> raise Failure "empty list"
        | [ one ] -> [ diff (one, x) ]
        | hd :: tl -> diff (hd, x) :: diffForExpList tl
      in
      Sum (diffForExpList li)
