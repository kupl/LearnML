
(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> let funa p = match p with | [] -> [] | hd::tl -> hd in
		match e with | Const(a) -> Const 0
			    | Var(a) -> if a!=x then Const 0 else Const 1
			    | Power(a,b) -> if a!=x then Const 0 else Times (Const b :: Power(a ,(b-1)) :: [])
			    | Times k -> (match k with | hd::tl -> Times [hd::(diff (funa tl) x)])
 			    | Sum k -> (match k with | hd::tl -> Sum (hd :: Sum tl));;