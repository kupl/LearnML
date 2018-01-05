module type ZEXPR =
sig
	exception ERROR of string
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
	val int_of_value: value -> int
end

module Zexpr =
struct
	exception ERROR of string
	type id = string
	type expr = NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr
		| MULT of expr * expr
		| DIVIDE of expr * expr
		| MAX of expr list
		| VAR of id
		| LET of id * expr * expr
	type environment = ((id * value) list)
	and value = VAL of int

	let int_of_value v = match v with VAL i -> i

	let plus(v0,v1) = VAL((int_of_value v0) + (int_of_value v1))
	let minus(v0,v1) = VAL((int_of_value v0) - (int_of_value v1))
	let mult(v0,v1) = VAL((int_of_value v0) * (int_of_value v1))
	let divide(v0,v1) = VAL((int_of_value v0) / (int_of_value v1))
	let biggerThan(v0,v1) = (int_of_value v0) > (int_of_value v1)

	let emptyEnv = []
	let rec checkEnv(env,x) =
		match env with
		[] -> raise (ERROR "FreeVariable")
		| p::others -> 
			if (fst p) = x then snd p
			else checkEnv(others,x)
	let rec eval (env, e) =
		match e with
		NUM i -> VAL i
		| PLUS(e0,e1) -> plus((eval (env,e0)), (eval (env,e1)))
		| MINUS(e0,e1) -> minus((eval (env,e0)), (eval (env,e1)))
		| MULT(e0,e1) -> mult((eval (env,e0)), (eval (env,e1)))
		| DIVIDE(e0,e1) -> divide((eval (env,e0)), (eval (env,e1)))
		| MAX [] -> VAL 0
		| MAX(left::others) -> eval(env,max(env, others, left))
		| VAR x -> checkEnv(env, x)
		| LET(x,v,e) -> eval((x,eval(env,v))::env,e)
	and max (env, l, m) =
		match l with
		[] -> m
		| left::others ->
			if biggerThan(eval(env,left), eval(env,m)) then max(env, others, left)
			else max(env, others, m)
end
