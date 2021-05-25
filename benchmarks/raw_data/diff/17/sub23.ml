(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
  Const k -> (Const 0)
| Var v -> 
    if x = v then (Const 1)
    else (Const 0)
| Power (v,n) ->
    if x = v then (Times[(Const n); (Power (v, n-1))])
    else (Const 0)
| Times te ->( match te with
      [] -> (Const 0)
    | hd::tl -> (Sum[ Times ([diff (hd,x)] @ tl); Times (hd :: [diff ((Times tl),x)]) ])
    )
| Sum se ->( match se with
      [] -> (Const 0)
    | hd::tl -> (Sum[diff (hd,x); diff ((Sum tl),x)])
    )
