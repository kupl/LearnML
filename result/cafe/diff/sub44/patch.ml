type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Sum list1 -> (
      match list1 with
      | [] -> Const 0
      | hd :: tl -> Sum [ diff (hd, x); diff (Sum tl, x) ] )
  | Const x -> Const 0
  | Var y -> if y != x then Const 0 else Const 1
  | Power (z1, z2) ->
      if z1 != x then Const 0 else Times [ Const z2; Power (z1, z2 - 1) ]
  | Times list2 -> (
      match list2 with
      | [] -> Const 0
      | __s63 :: __s64 ->
          Sum
            [
              Times (diff (__s63, x) :: __s64);
              Times [ __s63; diff (Times __s64, x) ];
            ] )
