type env = (string * int) list

exception Invalid

(* Input checker *)
let rec gather_vars : aexp -> string list -> string list
= fun aexp vars ->
	match aexp with
	| Const n -> vars
	| Var x | Power (x, _) -> x::vars
	| Times l | Sum l -> List.fold_left (fun vars aexp -> gather_vars aexp vars) vars l

let rec gather_vars2 : env -> string list -> string list
= fun env vars ->
	match env with
	| [] -> vars
	| (x, v)::tl -> gather_vars2 tl (x::vars)

let rec all_bound : string list -> string list -> bool
= fun vars1 vars2 ->
	match vars1 with
	| [] -> true
	| hd::tl -> if (List.mem hd vars2) then all_bound tl vars2 else false

let input_check : aexp -> env -> bool
= fun aexp env ->
	let (vars1, vars2) = (gather_vars aexp [], gather_vars2 env []) in
	all_bound vars1 vars2
	
let rec find_env : env -> string -> int
= fun env x ->
	match env with
	| [] -> raise (Failure (x ^ "Not Found"))
	| (y, v)::tl -> if (y = x) then v else find_env tl x

let rec aexp_eval : aexp -> (string * int) list -> int
= fun e env ->
	match e with
	| Const n -> n
	| Var x -> find_env env x 
	| Power (x, n) -> 
		if n <= 0 then 1 else (find_env env x) * (aexp_eval (Power (x, n-1)) env) (* ??? *)
	| Times l ->
		begin 
			match l with
			| [] -> 0
			| [hd] -> aexp_eval hd env
			| hd::tl -> (aexp_eval hd env) * (aexp_eval (Times tl) env)
		end
	| Sum l -> 
		begin
			match l with
			| [] -> 0
			| [hd] -> aexp_eval hd env
			| hd::tl -> (aexp_eval hd env) + (aexp_eval (Sum tl) env)
		end

let grading : (aexp * string) -> env -> int
= fun (aexp, str) env -> 
	let diff_result = diff (aexp, str) in
	if input_check aexp env then aexp_eval diff_result env else raise Invalid
