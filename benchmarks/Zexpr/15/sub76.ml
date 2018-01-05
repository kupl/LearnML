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

module Environment =
struct
  exception NoMatchingVariable

  type id = string
  type value = int
  type variable = Variable of id * value
  type environment = variable list

  let emptyEnv: environment = []

  let insert_variable ((env: environment), (var: variable)) =
    var :: env

  let rec find_variable ((env: environment), (variable_name: id)) : value =
    match env with
      [] -> raise NoMatchingVariable
    | h :: rest ->
      match h with Variable(k, v) ->
        if k = variable_name then v
        else find_variable (rest, variable_name)
end

module Zexpr : ZEXPR =
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

  module E = Environment

  type environment = E.environment
  type value = int

  let emptyEnv = E.emptyEnv

  let rec eval ((env: environment), (expression: expr)) : value =
    match expression with
      NUM n -> n
    | PLUS (e1, e2) ->
      let v1 = eval(env, e1) in
      let v2 = eval(env, e2) in
      v1 + v2
    | MINUS (e1, e2) ->
      let v1 = eval(env, e1) in
      let v2 = eval(env, e2) in
      v1 - v2
    | MULT (e1, e2) ->
      let v1 = eval(env, e1) in
      let v2 = eval(env, e2) in
      v1 * v2
    | DIVIDE (e1, e2) ->
      let v1 = eval(env, e1) in
      let v2 = eval(env, e2) in
      v1 / v2
    | MAX lst ->
      let rec list_maximum (expression_list: expr list) =
        match expression_list with
          [] -> 0
        | h :: rest ->
          let rest_max = list_maximum rest in
          let cur_value = eval (env, h) in
          if cur_value > rest_max then cur_value
          else rest_max
      in list_maximum lst
    | VAR variable_name ->
      (try E.find_variable (env, variable_name) with
        E.NoMatchingVariable -> raise (Error "FreeVariable"))
    | LET (variable_name, e1, e2) ->
      let v1 = eval(env, e1) in
      eval(E.insert_variable(env, E.Variable(variable_name, v1)), e2)

  let print_value (v: value) =
    print_int v; print_string "\n"; ()
end;;
