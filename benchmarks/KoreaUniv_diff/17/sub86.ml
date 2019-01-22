(*problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
 
let rec diff : aexp * string -> aexp
= fun (e,x) ->
match e with
| Const n -> Const 0
| Var a -> let is_x a x = if a = x then Const 1 else Const 0 in is_x a x
| Power (a, n) -> if a = x then let diff_var (a, n) = Times[Const n; Power(a, n - 1)] in diff_var (a,n) else Const 0
| Times [] -> Const 0
| Times (hd::tl) -> if tl = [] then diff (hd, x) else Sum[Times(diff(hd, x)::tl); Times [hd;diff(Times tl, x)]]
| Sum [] -> Const 0
| Sum (hd::tl) -> if tl = [] then diff (hd, x) else Sum[diff(hd, x); diff(Sum tl, x)]
 