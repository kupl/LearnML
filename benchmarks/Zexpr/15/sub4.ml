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
  type environment = (id * value) list

  let emptyEnv = []
  let rec eval (environment, expression) =
    let rec get_variable environment name =
      match environment with
      | [] -> raise (Error "FreeVariable")
      | (variable_name, variable_value)::tail -> if name = variable_name then
                                                   variable_value
                                                 else 
                                                   get_variable tail name
    in
      let rec set_variable environment name value =
        match environment with
        | [] -> [(name, value)]
        | (variable_name, _)::tail -> if name = variable_name then
                                        (name, value)::tail
                                      else 
                                        set_variable tail name value
      in
        match expression with
        | NUM number -> number
        | PLUS (lhs, rhs) -> (eval (environment, lhs))
                             + (eval (environment, rhs))
        | MINUS (lhs, rhs) -> (eval (environment, lhs))
                              - (eval (environment, rhs))
        | MULT (lhs, rhs) -> (eval (environment, lhs))
                             * (eval (environment, rhs))
        | DIVIDE (lhs, rhs) -> (eval (environment, lhs))
                               / (eval (environment, rhs))
        | MAX [] -> 0
        | MAX (head::[])-> eval (environment, head)
        | MAX (head::tail) -> let head_value = eval (environment, head)
                              in
                                let tail_value = eval (environment, MAX tail)
                                in
                                  if head_value >= tail_value then
                                    head_value
                                  else
                                    tail_value
        | VAR variable_name -> get_variable environment variable_name
        | LET (variable_name, exp1, exp2) ->
            let variable_value = eval (environment, exp1)
            in
              let new_environment = set_variable environment variable_name
                                                 variable_value
              in
                eval (new_environment, exp2)

  let print_value value =
    print_endline (string_of_int value)
end
