(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
	| Const n -> Const 0
	| Var var when var = x -> Const 1
	| Var var -> Const 0 (* when var <> x *)
	| Power (var, n) when var = x -> 
		if n = 0 then Const 0
		else Times [Power (var, (n-1)); Const n]
	| Power (var, n) (* when var <> x *) -> Const 0
	| Sum list -> (
		match list with
		| [] -> raise (Failure "NullListError: list argument is null")
		| hd :: tl when tl <> [] -> Sum (diff (hd, x) :: [(diff ((Sum tl), x))])
		| hd :: tl (* when tl = [] *) -> (diff (hd, x))
	)
	| Times list -> (
		match list with
		| [] -> raise (Failure "NullListError: list argument is null")
		| hd :: tl when tl <> [] -> Sum ((Times ((diff (hd, x)) :: tl)) :: [Times (hd :: [(diff ((Times tl), x))])])
		| hd :: tl (* when tl = [] *) -> (diff (hd, x))
	)