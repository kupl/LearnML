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

	type value = 
		| VALID of int
		| INVALID
	type environment = (id * value) list

	let valplus : value * value -> value = fun (val1, val2) ->
		match (val1, val2) with
		| (_, INVALID) -> INVALID
		| (INVALID, _) -> INVALID
		| (VALID i1, VALID i2) -> VALID (i1+i2)
	
	let valminus : value * value -> value = fun (val1, val2) ->
		match (val1, val2) with
		| (_, INVALID) -> INVALID
		| (INVALID, _) -> INVALID
		| (VALID i1, VALID i2) -> VALID (i1-i2)
	
	let valmult : value * value -> value = fun (val1, val2) ->
		match (val1, val2) with
		| (_, INVALID) -> INVALID
		| (INVALID, _) -> INVALID
		| (VALID i1, VALID i2) -> VALID (i1*i2)
	
	let valdivide : value * value -> value = fun (val1, val2) ->
		match (val1, val2) with
		| (_, INVALID) -> INVALID
		| (INVALID, _) -> INVALID
		| (VALID i1, VALID i2) -> 
			if i2 = 0 then raise (Error "ZeroDivision")
			else VALID (i1/i2)

	let rec isin : environment * id -> bool = fun (env, id1) ->
		match env with
		| [] -> false
		| hd::tl ->
			if (fst hd) = id1 then true
			else isin (tl, id1)

	let rec valfromenv : environment * id -> value = fun (env, id1) ->
		match env with
		| [] -> INVALID
		| hd::tl ->
			if (fst hd) = id1 then snd hd
			else valfromenv (tl, id1)
	
	let rec updateenv : environment * id * value -> environment = fun (env, id1, val1) ->
		if isin (env, id1) then 
			(
			let h = List.hd env in
			let t = List.tl env in
			if (fst h) = id1 then
				(let p = (id1,val1) in
				p::t)
			else h::updateenv(t, id1, val1)
			)
		else 
			(let p = (id1, val1) in
			p::env)

  	let emptyEnv : environment = [] 
  	let rec eval : environment * expr -> value = fun (env, exp) ->
		match exp with
		| NUM i -> VALID i
		| PLUS (exp1, exp2) -> valplus (eval(env, exp1), eval(env, exp2))
		| MINUS (exp1, exp2) -> valminus (eval(env, exp1), eval(env, exp2))
		| MULT (exp1, exp2) -> valmult (eval(env, exp1), eval(env, exp2))
		| DIVIDE (exp1, exp2) -> valdivide (eval(env, exp1), eval(env, exp2))
		| MAX expl -> 
			let rec valmax : environment * expr list * value -> value = fun (env, expl, val1) ->
		match expl with
				| [] -> val1
				| hd::tl -> 
					let val2 = eval(env, hd) in
					(
					match (val1, val2) with
					| (_, INVALID) -> valmax(env, tl, val1)
					| (INVALID, _) -> valmax(env, tl, val2)
					| (VALID i1, VALID i2) ->
						if i1 > i2 then valmax(env, tl, val1)
						else valmax(env, tl, val2)
					)
			in
			(match expl with
			| [] -> VALID 0
			| _ -> valmax (env, expl, INVALID))
		| VAR id1 ->
			(if isin(env, id1) then valfromenv(env, id1)
			else raise (Error "FreeVariable"))
		| LET (id1, exp1, exp2) ->
			(let env_new = updateenv(env, id1, eval(env, exp1)) in
			eval(env_new, exp2))

  	let print_value : value -> unit = fun vl ->
		match vl with
		| INVALID -> raise (Error "InvalidValue")
		| VALID i -> 
			print_string (string_of_int i ^ "\n")
end
