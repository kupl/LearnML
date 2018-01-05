module type ZEXPR = 
	sig 
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

	val emptyEnv : environment 
	val eval : environment * expr -> value 

	val print_value : value -> unit 
	end 

module Zexpr : ZEXPR = 
	struct
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
	type environment = (id, value) Hashtbl.t
	
	let emptyEnv = Hashtbl.create 10

	let rec max = fun (env, m, l) ->
		match l with 
		|[] -> m 
		|e::ll -> (
			let c = eval (env, e) in
			if c>m then max (env, c, ll)
			else max(env, m, ll))

	and eval = fun (env, e) ->
		match e with
		|NUM n -> n
		|PLUS (a, b) -> eval(env, a) + eval(env, b)
		|MINUS (a, b) -> eval(env, a) - eval(env, b)
		|MULT (a, b) -> eval(env, a) * eval(env, b)
		|DIVIDE (a, b) -> eval(env, a) / eval(env, b)
		|MAX l -> (match l with 
			|[] -> 0
			|a::ll -> max (env, eval (env, a), ll))
		|VAR i -> (
			if Hashtbl.length env = 0 then raise (Error "FreeVariable")
			else Hashtbl.find env i
			)
		|LET (i, v, s)-> (Hashtbl.add env i (eval (env, v)); 
			eval (env, s))


	let print_value = fun v -> print_endline(string_of_int v)

	end;;

