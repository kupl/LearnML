type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff ((aexp : aexp), (x : string)) : aexp =
  match aexp with
  | Const c -> Const 0
  | Var a -> if a = x then Const 1 else Const 0
  | Power (a, n) -> Times [ Const n; Power (a, n - 1) ]
  | Times l -> Sum (help_times (l, l, 0, x))
  | Sum l -> Sum (help_sum (l, x))


and help_sum ((l : aexp list), (x : string)) : aexp list =
  match l with [] -> [] | h :: t -> diff (h, x) :: help_sum (t, x)


and help_times1 ((l : aexp list), (n : int), (x : string)) : aexp list =
  match l with
  | [] -> []
  | h :: t ->
      if n = 0 then help_times1 (t, n - 1, x) else h :: help_times1 (t, n - 1, x)


and help_times ((ll : aexp list), (l : aexp list), (n : int), (x : string)) :
    aexp list =
  match l with
  | [] -> []
  | h :: t ->
      Times (diff (h, x) :: help_times1 (ll, n, x))
      :: help_times (ll, t, n + 1, x)
