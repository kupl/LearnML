type env = (string * int) list

exception Invalid

(* Input checker *)
let rec gather_vars : ae -> string list -> string list
= fun ae vars ->
	match ae with
	| CONST n -> vars
	| VAR x | POWER (x, _) -> x::vars
	| TIMES l | SUM l -> List.fold_left (fun vars ae -> gather_vars ae vars) vars l

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

let input_check : ae -> env -> bool
= fun ae env ->
	let (vars1, vars2) = (gather_vars ae [], gather_vars2 env []) in
	all_bound vars1 vars2
	
let rec find_env : env -> string -> int
= fun env x ->
	match env with
	| [] -> raise (Failure (x ^ "Not Found"))
	| (y, v)::tl -> if (y = x) then v else find_env tl x

let rec ae_eval : ae -> (string * int) list -> int
= fun e env ->
	match e with
	| CONST n -> n
	| VAR x -> find_env env x 
	| POWER (x, n) -> 
		if (n <= 0) then 1 else (find_env env x) * (ae_eval (POWER (x, n-1)) env) (* ??? *)
	| TIMES l ->
		begin 
			match l with
			| [] -> 0
			| [hd] -> ae_eval hd env
			| hd::tl -> (ae_eval hd env) * (ae_eval (TIMES tl) env)
		end
	| SUM l -> 
		begin
			match l with
			| [] -> 0
			| [hd] -> ae_eval hd env
			| hd::tl -> (ae_eval hd env) + (ae_eval (SUM tl) env)
		end

let grading : (ae * string) -> env -> int
= fun (ae, str) env -> 
	let diff_result = diff (ae, str) in
	if input_check ae env then ae_eval diff_result env else raise Invalid
