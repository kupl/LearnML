(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
;;

let rec diff : aexp * string -> aexp
= fun (e,x) ->
  match e with
  | Const _ -> Const 0
  | Var y -> if x = y then Const 1 else Const 0
  | Power (y, z) -> if not (x = y) then Const 0
                    else Times [Const z; Power (y, z - 1)]
  | Times [] -> Const 0
  | Times (hd::tl) -> Sum [Times (diff (hd, x)::tl); Times [hd; diff(Times tl, x)]]
  | Sum aexps -> Sum (List.map (fun ae -> diff (ae, x)) aexps)
;;