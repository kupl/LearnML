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

  type environment = ( string * int ) list
  type value = int

  let emptyEnv = []
  let rec eval = fun (env,e) ->
    match e with
    | NUM i -> i
    | PLUS (e1,e2) -> eval(env,e1)+eval(env,e2)
    | MINUS (e1,e2) -> eval(env,e1)-eval(env,e2)
    | MULT (e1,e2) -> eval(env,e1)*eval(env,e2)
    | DIVIDE (e1,e2) -> eval(env,e1)/eval(env,e2)
    | VAR i ->
        if List.mem_assoc i env then
          List.assoc i env
        else raise(Error "FreeVariable")
    | LET (i,e1,e2) ->
        if List.mem_assoc i env then
          eval((i,eval(env,e1))::List.remove_assoc i env,e2)
        else eval((i,eval(env,e1))::env,e2)
    | MAX el -> (
      match el with
        | [] -> 0
        | [e] -> eval(env,e)
        | e::l ->
          if eval(env,e) > eval(env,MAX l) then eval(env,e)
          else eval(env,MAX l)
    )
  let print_value = fun v -> print_int(v);print_endline("");
end
