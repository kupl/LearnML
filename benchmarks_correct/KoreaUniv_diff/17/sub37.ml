(* ------------------problem4------------------ *)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> 
	let rec sum l = 
		match l with
		| [] -> Const 0
		| hd::tl -> Sum([(diff (hd, x)) ; (sum tl)]) in

	let rec times l = 
		match l with
		| [] -> Const 0
		| hd::tl -> 
			Sum(Times ((diff (hd, x)) ::tl) :: Times ([hd ; (times tl)]) :: []) in

	match e with 
	| Const(n) -> Const 0
	| Var(s) -> if s=x then Const 1 else Const 0
	| Power(s,n) -> if s=x then Times[Const n; Power (s, (n-1))] 
					else Const 0 
	| Times(l) -> times l
	| Sum(l) -> sum l;;
(* -------------------------------------------- *)