(* 2012-11230 Kim sangmin *)

module type ZEXPR =
sig
	exception Error of string
	type id = string
	type expr =
		| NUM of int
		| PLUS of expr*expr
		| MINUS of expr*expr
		| MULT of expr*expr
		| DIVIDE of expr*expr
		| MAX of expr list
		| VAR of id
		| LET of id*expr*expr

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
		| PLUS of expr*expr
		| MINUS of expr*expr
		| MULT of expr*expr
		| DIVIDE of expr*expr
		| MAX of expr list
		| VAR of id
		| LET of id*expr*expr
	type environment = (string*int) list
	type value = int

	let emptyEnv = []
	let rec eval = fun(env, exp) ->
		match exp with
		| NUM i -> i
		| PLUS(i,j) -> eval(env, i) + eval(env, j)
		| MINUS(i,j) -> eval(env, i) - eval(env, j)
		| MULT(i,j) -> eval(env, i) * eval(env, j)
		| DIVIDE(i,j) -> eval(env, i) / eval(env, j)
		| MAX(l) -> (
					match l with
					| [] -> 0
					| _ -> (
							let rec find_max = fun(exp_list, max_val) -> 
								match exp_list with
								| [] -> max_val
								| _ -> (
										let tmp = eval(env, List.hd(exp_list)) in
										if(tmp > max_val) then find_max(List.tl(exp_list), tmp)
										else find_max(List.tl(exp_list), max_val)
									)
							in
							find_max(List.tl(l), eval(env, List.hd(l)))
							)
					)
		| VAR(i) -> (
					let rec find = fun(env, item) ->
						match env with
						| [] -> raise(Error "FreeVariable")
						| (hd::tl) -> if((compare (fst(hd)) item)=0) then snd(hd)
									  else find(tl, item)
					in
					find(env, i)
				)
		| LET(i, e1, e2) -> (
							let tmp = eval(env, e1) in
							eval((i,tmp)::env, e2)
							)	
	let print_value = fun(x) -> print_int(x);print_newline()
end

