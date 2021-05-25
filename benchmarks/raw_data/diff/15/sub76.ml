(* Problem 4 *)

type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list;;

let rec diff(axep, x) = match axep with
  | Const n -> Const 0
  | Var y -> if x=y then Const 1 else Const 0
  | Power (z,m) -> if x=z then (if m=2 then Times [Const m; Var x] else Times [Const m; Power (x,m-1)]) else Const 0
  | Times (h::t) -> Sum [Times (diff (h,x)::t); Times [h; diff (Times t,x)]]
  | Times [] -> Const 0
  | Sum (hd::tl) -> Sum [diff (hd,x); diff (Sum tl,x)] 
  | Sum [] -> Const 0;;

