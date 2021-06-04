type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((exp : aexp), (x : string)) : aexp =
  match exp with
  | Const a -> Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Power (a, b) ->
      if a = x then Times [ Const b; Power (a, b - 1) ] else Const 0
  | Times l -> diff_times (l, x)
  | Sum l -> Sum (diff_sum (l, x))


and diff_times ((l : aexp list), (x : string)) : aexp =
  match l with
  | [] -> Const 0
  | h :: t ->
      Sum ([ Times (diff (h, x) :: t) ] @ [ Times [ h; diff_times (t, x) ] ])


and diff_sum ((l : aexp list), (x : string)) : aexp list =
  match l with [] -> [] | h :: t -> diff (h, x) :: diff_sum (t, x)
