exception FreeVariable

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

    let rec maxl: environment -> expr list -> value = fun env l ->
      match l with
      | [] -> 0
      | h::[] -> eval (env, h)
      | h::t -> max (eval (env, h)) (maxl env t)

    and eval: environment * expr -> value = fun (env, exp) ->
      match exp with
      | NUM a -> a
      | PLUS (a, b) -> (eval (env, a)) + (eval (env, b))
      | MINUS (a, b) -> (eval (env, a)) - (eval (env, b))
      | MULT (a, b) -> (eval (env, a)) * (eval (env, b))
      | DIVIDE (a, b) -> (eval (env, a)) / (eval (env, b))
      | MAX l -> maxl env l
      | VAR a ->
       (match env with
	| [] -> raise FreeVariable
	| (id, value)::t -> if (id = a) then value
			    else (eval (t, exp)))
      | LET (id, a, b) -> eval ((id, eval (env, a))::env, b)

    let print_value value = print_int value; print_string "\n"
  end
