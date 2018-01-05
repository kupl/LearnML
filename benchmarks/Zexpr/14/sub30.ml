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
	
	val emptyEnv: environment
	val eval: environment * expr -> value

	val int_of_value: value -> int
end

module Zexpr =
struct
	exception Error of string
	type id = string
	and expr =
	| NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list
	| VAR of id
	| LET of id * expr * expr
	and value = int
	and iv = id * value
	and environment = iv list

	let int_of_value (v: value): int = v
	let emptyEnv = []
	let rec eval ((env: environment), (ep: expr)): value =
		match ep with
		| NUM n -> n
		| PLUS (m, n) -> (eval (env, m)) + (eval (env, n))
		| MINUS (m, n) -> (eval (env, m)) - (eval (env, n))
		| MULT (m, n) -> (eval (env, m)) * (eval (env, n))
		| DIVIDE (m, n) -> (
			let p, q = eval (env, m), eval (env, n) in
			if (q == 0) then raise (Error "Divison_by_Zero")
			else p / q 
			)
		| MAX el -> (
			let values = (List.sort (fun m n -> n - m)
			(List.map (fun eachExp -> eval (env, eachExp)) el)) in (
			match values with
			| [] -> 0
			| hd::tl -> hd
			))
		| LET (x, m, n) -> (
			let newVal = eval (env, m) in
			let newIV = (x, newVal) in
			(eval (newIV::env, n))
			)
		| VAR i -> (
			let matchingIVs = List.filter (fun eachIv -> ((fst eachIv) = i)) env in
			match matchingIVs with
			| [] -> raise (Error "FreeVariable")
			| hd::tl -> (snd hd)
			)
end


