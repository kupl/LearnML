module type ZEXPR =
	sig
		exception Error of string
		type id = string
		type expr = NUM of int
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
		val print_value: value -> unit
	end

module Zexpr : ZEXPR = 
	struct
		exception Error of string
		type id = string
		type expr = NUM of int
			| PLUS of expr * expr
			| MINUS of expr * expr
			| MULT of expr * expr
			| DIVIDE of expr * expr
			| MAX of expr list
			| VAR of id
			| LET of id * expr * expr	
		type value = int 
		type environment = (id * value) list
		let emptyEnv: environment = []
		let eval: environment * expr -> value = 
			fun(env, exp) -> 
				let rec rec_calc(_env: environment)( _exp: expr): value =
					match _exp with
					| NUM(i) -> i
					(*is it possible to polymize the function but use it differently*)
					(*NOPE*)
					| PLUS(l,r) -> rec_calc(_env)(l) + rec_calc(_env)(r)
					| MINUS(l,r) -> rec_calc(_env)(l) - rec_calc(_env)(r)
					| MULT(l,r) -> rec_calc(_env)(l) * rec_calc(_env)(r)
					| DIVIDE(l,r) -> rec_calc(_env)(l) / rec_calc(_env)(r)
					| MAX(e) -> begin 
						match List.map(rec_calc(_env))(e) with
						| [] -> 0
						| x::xs  -> List.fold_left max x xs
						end 
					(*https://rosettacode.org/wiki/Greatest_element_of_a_list#OCaml*)
					(*map : ('a -> 'b) -> 'a list -> 'b list*)
					| VAR(i) -> begin
						try List.assoc(i)(_env) with 
							| (Not_found)->raise(Error "FreeVariable")
						end
					| LET (i, e1, e2) -> rec_calc((i,rec_calc(_env)(e1))::_env)(e2)
			in
			rec_calc(env)(exp)
		let print_value(n: value): unit = print_int(n); print_string("\n")
	end


