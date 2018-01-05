(* hw2-6, 2012-11259 *)

module type ZEXPR =
sig
  exception Error of string
  type id = string
  type expr =
    | NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DEVIDE of expr * expr
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
  type expr =
    | NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DEVIDE of expr * expr
	| MAX of expr list
	| VAR of id
	| LET of id * expr * expr
  type environment = int list
  type value = int

  let emptyEnv: environment = []

  let rec change: id * expr -> expr -> expr =
    fun (id, e') e -> 
      let change' = change (id, e') in
      match e with
      | NUM i -> NUM i
      | PLUS(e1, e2) -> PLUS(change' e1, change' e2)
      | MINUS(e1, e2) -> MINUS(change' e1, change' e2)
      | MULT(e1, e2) -> MULT(change' e1, change' e2)
      | DEVIDE(e1, e2) -> DEVIDE(change' e1, change' e2)
      | MAX l -> MAX (List.map change' l)
      | VAR id' -> if id = id' then e' else e
      | LET (id, e1, e2) -> change' (change (id, e1) e2)

  let rec eval: environment * expr -> value =
    fun (en, e) ->
      let eval' x = eval (en, x) in
      let compare' a b = compare b a in
      match e with
	  | NUM i -> i
	  | PLUS(e1, e2) -> eval' e1 + eval' e2
	  | MINUS(e1, e2) -> eval' e1 - eval' e2
	  | MULT(e1, e2) -> eval' e1 * eval' e2
	  | DEVIDE(e1, e2) -> eval' e1 / eval' e2
	  | MAX l -> if l = [] then 0
        else List.hd (List.sort compare' (List.map eval' l))
	  | VAR id -> raise (Error "FreeVariable")
	  | LET(id, e1, e2) -> eval' (change (id, e1) e2)

  let int_of_value: value -> int =
    fun v -> v
end
