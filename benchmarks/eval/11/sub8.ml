(* hw3. *)
type expr = NUM of int
		  | PLUS of expr * expr
		  | MINUS of expr * expr
		  | MULT of expr * expr
		  | DIVIDE of expr * expr
		  | MAX of expr list
(* eval: expr -> int *)
let rec eval expr =
	match expr with
	 NUM n -> n
	|PLUS(e1, e2) -> eval e1 + eval e2
	|MINUS(e1, e2) -> eval e1 - eval e2
	|MULT(e1, e2) -> eval e1 * eval e2
	|DIVIDE(e1, e2) -> eval e1 / eval e2
	|MAX lst -> 
		(match lst with
		  [] -> 0
		 |hd::[] -> eval hd
		 |hd1::hd2::tl ->
		 	if eval hd1 > eval hd2 then eval (MAX(hd1::tl))
			else eval (MAX(hd2::tl)))
(*
let _ = eval (MAX [NUM 1; NUM 3; NUM 2; NUM 8; NUM 7])
*)
