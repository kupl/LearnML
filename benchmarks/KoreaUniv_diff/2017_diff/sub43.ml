(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> (* TODO *)
	let rec subdiff e x =
	match e with
	| Const _ -> Const 0
	| Var s -> if s=x then Const 1 else Const 0
	| Power (s,n) -> if s=x then Times[Const n;Power (s,n-1)] else Const 0
	| Times al ->
		let rec sub b =
			match b with
			| [] -> Const 0
			| hd::tl -> Sum[Times ((subdiff hd x)::tl); Times[hd;(sub tl)]]
		in sub al
	| Sum al ->
		let rec sub b =
			match b with
			| [] -> []
			| hd::tl -> (subdiff hd x)::(sub tl)
		in Sum (sub al)
	in subdiff e x
