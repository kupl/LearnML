type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (var : string)) : aexp =
  match aexp with
  | Const x -> Const 0
  | Var x -> if x != var then Const 0 else Const 1
  | Power (x, i) ->
      if x != var then Const 0
      else if i = 1 then Const 1
      else Times [ Const i; Power (x, i - 1) ]
  | Times aexplist -> (
      match aexplist with
      | [] -> Const 0
      | hd :: tl ->
          Sum
            [ Times (diff (hd, var) :: tl); Times [ hd; diff (Times tl, var) ] ]
      )
  | Sum aexplist -> Sum (List.map (fun (x : aexp) -> diff (x, var)) aexplist)
