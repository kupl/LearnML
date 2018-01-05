(* hw2-6 *)

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
  val int_of_value : value -> int
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

  type environment = (id * value) list

  let emptyEnv : environment = []

  let rec envValue (i : id) (env : environment) : value =
    match env with
    | [] -> raise (Error "FreeVariable")
    | lh::lt ->
      if fst lh = i then
        snd lh
      else
        envValue i lt

  let rec eval ((env, exp) : (environment * expr)) : value =
    match exp with
    | NUM i -> i
    | PLUS (a, b) -> (eval (env, a)) + (eval (env, b))
    | MINUS (a, b) -> (eval (env, a)) - (eval (env, b))
    | MULT (a, b) -> (eval (env, a)) * (eval (env, b))
    | DIVIDE (a, b) -> (eval (env, a)) / (eval (env, b))
    | MAX [] -> 0
    | MAX (a0::ai) ->
      List.fold_left
      (fun m a -> max m (eval (env, a)))
      (eval (env, a0))
      ai
    | VAR i -> envValue i env
    | LET (i, v, s) -> eval (((i, (eval (env, v)))::env), s)

  let int_of_value (v : value) : int =
    v

end
