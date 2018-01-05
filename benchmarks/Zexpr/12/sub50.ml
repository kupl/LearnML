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
	type environment = (id * int) list
	type value = unit
	let emptyEnv = []
	let eval =
		let rec vl = (fun ev ex ->
						match ex with
						| NUM a -> a
						| PLUS (a, b) -> (vl ev a) + (vl ev b)
						| MINUS (a, b) -> (vl ev a) - (vl ev b)
						| MULT (a, b) -> (vl ev a) * (vl ev b)
						| DIVIDE (a, b) -> (vl ev a) / (vl ev b)
						| MAX l -> (List.fold_left (fun a b -> (max a (vl ev b))) 0 l)
						| VAR s -> let rec findvar = (fun ev s -> (match ev with
													| (a, b):: t -> (if (a = s) then b else (findvar t s))
													| _ -> raise (Error "FreeVariable")
												)
									) in
								(findvar ev s)
						| LET (a, b, c) -> (vl ((a, (vl ev b)):: ev) c)
			) in
		
		(fun (env, exp) -> print_int(vl env exp))
end