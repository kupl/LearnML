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

  module Env = Map.Make(String)
  type environment = (int) Env.t
  type value = int

  let emptyEnv = Env.empty
  let rec eval = (fun (env,exp) ->
    let compareV = (fun v1 v2 ->
      v2 - v1
      ) in
    let evalCurrentEnv = (fun v ->
      (eval (env, v))
      ) in
    match exp with
      | NUM a -> a
      | PLUS (a,b) -> (eval (env,a)) + (eval (env,b))
      | MINUS (a,b) -> (eval (env,a)) - (eval (env,b))
      | MULT (a,b) -> (eval (env,a)) * (eval (env,b))
      | DIVIDE (a,b) -> (eval (env,a)) / (eval (env,b))
      | MAX l -> let sortedList = (List.sort compareV (List.map evalCurrentEnv l)) in
        if List.length sortedList > 0 then List.hd sortedList
        else 0
      | VAR str ->
        if Env.mem str env then Env.find str env
        else raise (Error "FreeVariable")
      | LET (id,value,ex) -> let newEnv = Env.add id (eval (env,value)) env in
        (eval (newEnv, ex))
  )

  let print_value = (fun value ->
    Printf.printf "%d\n" value
    )
end
