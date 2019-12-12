type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> match exp with
  | Const e -> Const 0
  | Var e ->
    if e = x then Const 1
    else Const 0
  | Power (s, e) ->
    if e = 0 then Const 0
    else if s <> x then Const 0
    else if e = 2 then Times [Const 2; Var s]
    else if e = 1 then Const 1
    else Times [Const e; Power (s, e - 1)]
  | Times l -> Sum (diff_times l [] x)
  | Sum l -> Sum (diff_sum l x)

and diff_times : aexp list -> aexp list -> string -> aexp list
= fun st ed x -> match st with
  | [] -> []
  | hd::tl -> (Times (ed@([(diff (hd, x))]@tl)))::(diff_times tl (ed@[hd]) x)
  
and diff_sum : aexp list -> string -> aexp list
= fun lst x -> match lst with
  | [] -> []
  | hd::tl -> (diff (hd, x))::(diff_sum tl x);;
  
(*
diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;

diff (Sum [Power ("x", 2); Times [Const 2; Var "x"; Var "y"]; Power("y", 2)], "x");;
*)