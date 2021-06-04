type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const integer -> Const 0
  | Var str -> if str != x then Const 0 else Const 1
  | Power (str, integer) ->
      if str != x then Const 0
      else Times ([ Const integer ] @ [ Power (str, integer - 1) ])
  | Times list1 -> (
      match list1 with
      | [] -> Const 0
      | hd :: tl ->
          Sum [ Times (diff (hd, x) :: tl); Times [ hd; diff (Times tl, x) ] ] )
  | Sum list2 -> (
      match list2 with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )


let (_ : aexp) = diff (Times [ Const 2; Var "x"; Const 2; Var "x" ], "x")
