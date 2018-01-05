module type ZEXPR = sig
    
    exception Error of string 
    type id = string
    type expr = 
        | NUM of int
        | PLUS of expr * expr
        | MINUS of expr * expr
        | MULT of expr * expr
        | DIVIDE of expr * expr
        | MAX of expr list
        | VAR of id
        | LET of id * expr * expr

    type environment
    type value
    
    val emptyEnv: environment
    val eval: environment * expr -> value
    
end

module Zexpr : ZEXPR = struct
    
    exception Error of string 
    type id = string
    type expr = 
        | NUM of int
        | PLUS of expr * expr
        | MINUS of expr * expr
        | MULT of expr * expr
        | DIVIDE of expr * expr
        | MAX of expr list
        | VAR of id
        | LET of id * expr * expr

    type value = int
    type environment = (id * value) list
	
    let emptyEnv = []
	
	(* IMPORTANT NOTICE ABOUT COPYRIGHT
			COPIED FROM 
				"sm5.ml" 
					OF
				(*
				 * SNU 4190.310 Programming Languages (Fall 2010)
				 *
				 * SM5
				 *)
	*)
	let (@?) l x = snd (List.find (fun y -> x = fst y) l)
	(* COPY END *)

    let eval (env, e) =
		let rec eval (env, e) = 
			match e with
				| NUM i -> i
				| PLUS (e_1, e_2) -> (eval (env, e_1)) + (eval (env, e_2))
				| MINUS (e_1, e_2) -> (eval (env, e_1)) - (eval (env, e_2))
				| MULT (e_1, e_2) -> (eval (env, e_1)) * (eval (env, e_2))
				| DIVIDE (e_1, e_2) -> (eval (env, e_1)) / (eval (env, e_2))
				| MAX l -> 
					let rec max_ (env, l) = (match l with
						| [] -> min_int
						| hd::tl -> max (eval (env, hd)) (max_ (env, tl))) in
					(match l with
						| [] -> 0
						| hd::tl -> max_ (env, l))
				| VAR id -> (try
					(env @? id)
					with Not_found -> raise (Error "Unbound Variable"))
				| LET (id, e1, e2) ->
					let y = eval (env, e1) in
					let env = (id, y)::env in
					eval (env, e2) in
		let ret = eval (env, e) in
		print_int (ret); print_newline();
		ret
end
    