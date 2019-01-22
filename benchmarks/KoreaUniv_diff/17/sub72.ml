(* problem 4*)
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
  | Var n -> if n = x then Const 1 else Const 0
  | Power (y , n) -> if y = x then Times [Const n ; Power (y, n-1)] else Const 0
  | Times lis1 -> begin
      match lis1 with
      |[] -> Const 0
      | hd::tl -> Sum[ Times ([diff (hd, x)]@tl) ; Times [ hd ; diff (Times tl, x)]]
    end
  | Sum lis2 ->
      match lis2 with
      | [] -> Const 0
      | hd::tl -> Sum [ diff (hd, x) ; diff (Sum tl, x)]
