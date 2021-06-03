type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> (*TODO*)
  match exp with
    | Sum [] -> Const 0
    | Times [] -> Const 0
    | Const n -> Const 0
    | Var v -> if (v = x) then Const 1
               else Const 0
    | Power (v, n) -> if (v = x) then Times [Const n; Power (x, n - 1)]
                      else Const 0
    | Times (hd::tl) -> Sum (Times (diff (hd, x)::tl) :: [Times [hd; diff (Times tl, x)]])
    | Sum (hd::tl) -> Sum (diff (hd, x) :: [diff (Sum tl, x)]);;
