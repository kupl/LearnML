(*
    PL 2-7
    2008-11609 박성원
*)

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

  type value = int
  type environment = (string * value) list

  let emptyEnv = []

  let eval = function (env, exp) ->

    let rec evalImpl = function (env, exp) ->
      match exp with
      | NUM n -> n
      | PLUS (e1, e2) -> (evalImpl (env, e1)) + (evalImpl (env, e2))
      | MINUS (e1, e2) -> (evalImpl (env, e1)) - (evalImpl (env, e2))
      | MULT (e1, e2) -> (evalImpl (env, e1)) * (evalImpl (env, e2))
      | DIVIDE (e1, e2) -> (evalImpl (env, e1)) / (evalImpl (env, e2))
      | MAX elist -> maxEvaluator (env, elist)
      | VAR id -> valExtract (id, env)
      | LET (id, e1, e2) -> evalImpl ((id, evalImpl (env, e1)) :: env, e2)

    and maxEvaluator = function (env, elist) ->
      match elist with
      | [] -> 0
      | exp :: [] -> evalImpl (env, exp)
      | exp :: elist ->
        let current = evalImpl (env, exp) in
        let prevMax = maxEvaluator (env, elist) in
        if current > prevMax then current else prevMax

    and valExtract = function (targetId, env) ->
      match env with
      | [] -> raise (Error "FreeVariable")
      | (id, value) :: env ->
        if id = targetId then value else valExtract (targetId, env)

    in
    evalImpl (env, exp)

  let print_value = function value -> print_int value; print_newline ()
end
