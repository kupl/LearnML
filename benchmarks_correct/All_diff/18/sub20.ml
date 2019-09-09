type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  match exp with
    | Const _ -> Const 0
    | Var a -> if a = x then (Const 1) else (Const 0)
    | Power (a, n) -> if a = x then (Times [Const n; Power (x, (n-1))]) else (Const 0)
    | Sum slst -> Sum (diff_sum slst x)
    | Times tlst -> Sum (diff_times [] tlst x)
and diff_sum : aexp list -> string -> aexp list
= fun slst x ->
  match slst with
    | [] -> []
    | hd::tl -> (diff (hd, x))::(diff_sum tl x)
and diff_times : aexp list -> aexp list -> string -> aexp list
= fun left right x ->
  match right with
    | [] -> []
    | hd::tl -> (Times ((left @ [diff (hd, x)]) @ tl))::(diff_times (left @ [hd]) tl x);;