type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (y : string)) : aexp =
  match aexp with
  | Const a -> Const 0
  | Var x -> if x = y then Const 1 else Const 0
  | Power (x, a) ->
      if x = y then
        if a = 0 then Const 0
        else if a = 1 then Const 1
        else Times [ Const a; Power (x, a - 1) ]
      else Const 0
  | Times l -> (
      match l with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, y) :: __s64);
              Times [ __s63; diff (Times __s64, y) ];
            ] )
  | Sum l -> (
      match l with
      | [ hd ] -> diff (hd, y)
      | hd :: tl -> Sum [ diff (hd, y); diff (Sum tl, y) ] )
