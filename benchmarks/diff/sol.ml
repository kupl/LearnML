exception Failure of string

type ae =
	| CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec map : (ae * string -> ae) -> ae list * string  -> ae list
= fun f (l, x) ->
	match l with
	| [] -> []
	| hd::tl -> (f (hd, x))::(map f (tl, x))

let rec diff : ae * string -> ae
= fun (e, x) ->
	match e with
	| CONST n -> CONST 0
	| VAR a -> if (a <> x) then CONST 0 else CONST 1
	| POWER (a, n) -> if (a <> x) then CONST 0 else TIMES [CONST n; POWER (a, n-1)]
	| TIMES l ->
		begin 
			match l with
			| [] -> CONST 0
			| hd::tl -> SUM [TIMES ((diff (hd, x))::tl); TIMES [hd; diff (TIMES tl, x)]]
		end
	| SUM l -> SUM (map diff (l, x))

(* grading function *)
type env = (string * int) list

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
		if (n = 0) then 1 else (find_env env x) * (ae_eval (POWER (x, n-1)) env)
	| TIMES l ->
		begin 
			match l with
			| [] -> raise (Failure "Invalid")
			| [hd] -> ae_eval hd env
			| hd::tl -> (ae_eval hd env) * (ae_eval (TIMES tl) env)
		end
	| SUM l -> 
		begin
			match l with
			| [] -> raise (Failure "Invalid")
			| [hd] -> ae_eval hd env
			| hd::tl -> (ae_eval hd env) + (ae_eval (SUM tl) env)
		end
