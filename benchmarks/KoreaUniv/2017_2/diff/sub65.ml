(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e, x) ->
  match e with
  | Const n -> Const 0
  | Var str -> if str = x then Const 1 else Const 0
  | Power (str, n) ->
      if str = x then Times [Const n; Power (x, (n-1))]
      else Const 0
  | Times l ->
    (match l with
    | [] -> Const 0
    | h::t -> Sum [Times (diff (h, x)::t);
                   Times [h; diff (Times t, x)]])
  | Sum l -> 
    (match l with
    | [] -> Const 0
    | h::t -> Sum [diff (h, x); diff (Sum t, x)])