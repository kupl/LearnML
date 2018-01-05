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

  type environment = (id * int) list 
  type value = int 

  let rec eval ((env : environment), (expr : expr)) = match expr with

  | NUM i -> i
  | PLUS (e1, e2) -> eval (env, e1) + eval (env, e2)
  | MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
  | MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
  | DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
  | MAX l -> let rec findMax cand lis = match lis with
  				| h::t -> let cand2 = eval (env, h) in if cand2 > cand then findMax cand2 t else findMax cand t
				| [] -> cand
				in (match l with
			| h::t -> findMax (eval (env, h)) t
			| [] -> 0)
  | VAR i -> let rec findVar (env, i) = match env with
  			| h::t -> if (fst h) = i then (snd h) else findVar (env, i)
			| [] -> raise (Error "FreeVariable")
			in findVar (env, i)
  | LET (id, e1, e2) -> eval ((id, eval (env, e1))::env, e2)

  let emptyEnv = []
  let print_value v = print_int v
end
