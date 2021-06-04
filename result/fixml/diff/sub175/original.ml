type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, x) ->
  match exp with
  | Const a -> Const 0
  | Var a -> if a = x then Const 1 else Var a
  | Power (a, b) ->
      if a = x then Times [ Const b; Power (a, b - 1) ] else Power (a, b)
  | Times l -> diff_times (l, x)
  | Sum l -> Sum (diff_sum (l, x))


and diff_times (l, x) =
  match l with
  | [] -> Const 0
  | h :: t ->
      Sum ([ Times (diff (h, x) :: t) ] @ [ Times [ h; diff_times (t, x) ] ])


and diff_sum (l, x) =
  match l with [] -> [] | h :: t -> diff (h, x) :: diff_sum (t, x)
