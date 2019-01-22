(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec change_elem : aexp list -> int -> aexp -> aexp list
= fun l i a ->
	match l with
	| hd::tl -> if i = 0 then a::tl else hd::(change_elem tl (i-1) a)
	| [] -> []
;;

let rec diff : aexp * string -> aexp
= fun (e,x) -> 
	match e with
	| Const i -> Const 0
	| Var v -> if x = v then Const 1 else Const 0
	| Power (v, i) -> (
		if x = v then (
			if i = 0 then Const 0
			else if i = 1 then Const 1
			else (
				let diff_pow = if i = 2 then Var v else (Power (v, (i-1))) in
				Times [(Const i); diff_pow] 
			)
		)
		else Const 0
	)
	| Times l -> (
		let result_list = ref [] in
		for i = 0 to (List.length l) - 1 do
			let temp_list = l in
			let ith_diff = diff ((List.nth l i), x) in
			let temp_list = (change_elem temp_list i ith_diff) in
			result_list := !result_list @ [Times temp_list]
		done;
		Sum !result_list
	)
	| Sum l -> (
		let result_list = ref [] in
		for i = 0 to (List.length l) - 1 do
			result_list := !result_list @ [diff ((List.nth l i), x)]
		done;
		Sum !result_list
	)
;;
