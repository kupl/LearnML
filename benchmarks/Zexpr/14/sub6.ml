module type ZEXPR =
sig
  exception Error of string
  type id = string
  type expr =
    NUM of int
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

  val int_of_value : value -> int
end

module Zexpr =
struct
  exception Error of string
  type id = string
  type expr =
    NUM of int
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

  let rec eval(env, exp: environment * expr): value = match exp with
    NUM(i) -> i
  | PLUS(e1, e2) -> eval(env, e1) + eval(env, e2)
  | MINUS(e1, e2) -> eval(env, e1) - eval(env, e2)
  | MULT(e1, e2) -> eval(env, e1) * eval(env, e2)
  | DIVIDE(e1, e2) -> eval(env, e1) / eval(env, e2)
  | MAX(exp) ->
    let evaluated = List.map (fun x -> eval(env, x)) exp in
    (
      match evaluated with
        h::t -> List.fold_left max h t
      | [] -> 0
    )
  | VAR(id) ->
    (
      try (
        (snd (List.find (fun iv -> (fst iv) = id) env))
      ) with Not_found -> raise (Error "FreeVariable")
    )
  | LET(id, value, exp) ->
    let evaluated = eval(env, value) in
    let env = (id, evaluated) :: env in
    eval(env, exp)

  let int_of_value(v: value): int = v
end
