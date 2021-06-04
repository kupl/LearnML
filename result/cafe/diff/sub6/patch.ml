type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (str : string)) : aexp =
  match aexp with
  | Const c -> Const 0
  | Var v -> if str = v then Const 1 else Const 0
  | Power (v, p) ->
      if v = str then Times [ Const p; Power (v, p - 1) ] else Const 0
  | Times aexpl -> (
      match aexpl with
      | [] -> Times []
      | [ __s63 ] -> diff (__s63, str)
      | hd :: tl ->
          Sum
            [ Times (diff (hd, str) :: tl); Times [ hd; diff (Times tl, str) ] ]
      )
  | Sum aexpl ->
      let diffwithstr (e : aexp) : aexp = diff (e, str) in
      Sum (List.map diffwithstr aexpl)
