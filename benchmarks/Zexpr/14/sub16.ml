(*
 * 컴퓨터공학부 2009-11690 김찬민
 * Homework 2 Exercise 6  *)

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

    type environment = (string * int) list
    type value = int

    let emptyEnv = []

    let rec searchVariable: (environment -> id -> value) =
        fun env vid ->
            match env with
            | (fid, fval) :: rest -> if fid = vid then fval else searchVariable rest vid
            | [] -> raise (Error "FreeVariable")

    let rec setVariable: (environment -> id -> value -> environment) =
        fun env vid vval ->
            match env with
            | (fid, fval) :: rest ->
                if fid = vid then (vid, vval) :: rest
                else (fid, fval) :: (setVariable rest vid vval)
            | [] -> (vid, vval) :: []

    let rec eval ((env, e) : (environment * expr)) : value =
        match e with
        | NUM(v) -> v
        | PLUS(e1, e2) -> eval(env, e1) + eval(env, e2)
        | MINUS(e1, e2) -> eval(env, e1) - eval(env, e2)
        | MULT(e1, e2) -> eval(env, e1) * eval(env, e2)
        | DIVIDE(e1, e2) -> eval(env, e1) / eval(env, e2)
        | MAX(first :: rest) -> (
            if rest = [] then eval(env, first)
            else max (eval(env, first)) (eval(env, MAX(rest)))
        )
        | MAX([]) -> 0
        | VAR(id) -> searchVariable env id
        | LET(id, assign, scope) ->
            let assignValue = eval(env, assign) in
            let newEnv = setVariable env id assignValue in
            eval (newEnv, scope)

    let int_of_value (v : value) : int = v
end
