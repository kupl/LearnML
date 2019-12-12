type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> match exp with
  | Const n -> Const 0
  | Var s -> Const 1
  | Power (str, pow) -> if pow = 1 then Const 1 else Times [Const pow; Power (str, pow-1)]
  | Times (lst) -> (match lst with
    | [] -> Const 0
    | hd::tl -> if tl <> [] then Sum[Times [diff (hd, x); (Times tl)]; Times[hd; diff ((Times tl), x)]] else diff (hd, x))
  | Sum (lst) -> match lst with
    | [] -> Const 0
    | hd::tl -> if tl <> [] then Sum[diff (hd, x); diff ((Sum tl), x)] else diff (hd, x);;