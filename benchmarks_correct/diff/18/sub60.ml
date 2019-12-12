type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
    match exp with
      | Const c -> Const 0
      | Var a ->
          if (a = x) then Const 1
          else Const 0
      | Power (a, n) ->
          if (a = x) then Times ( [Const n] @ [Power (a, n - 1)] )
          else Const 0
      | Times [] -> Const 0
      | Times (hd::tl) ->
          Sum ( [Times ([diff (hd, x)]@tl)] @ [Times ([hd]@[diff (Times tl, x)])] )
      | Sum [] -> Const 0
      | Sum (hd::tl) ->
          Sum ( [diff (hd, x)] @ [diff (Sum tl, x)] );;

(*
let test = Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1];;
diff (test, "x");;
*)
