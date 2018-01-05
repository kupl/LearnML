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
  module EnvMap =
    Map.Make(struct
      type t = id
      let compare : id -> id -> int = compare
    end)
  type environment = value EnvMap.t

  let emptyEnv = EnvMap.empty
  let rec eval ((env : environment), (e : expr)) : value =
    match e with
    | NUM n -> n
    | PLUS (e1, e2) -> eval(env, e1) + eval(env, e2)
    | MINUS (e1, e2) -> eval(env, e1) - eval(env, e2)
    | MULT (e1, e2) -> eval(env, e1) * eval(env, e2)
    | DIVIDE (e1, e2) -> eval(env, e1) / eval(env, e2)
    | MAX e_list -> (
        match e_list with
        | [] -> 0
        | hd::[] -> eval(env, hd)
        | hd::tl -> max (eval(env, hd)) (eval(env, MAX tl))
      )
    | VAR id -> (try EnvMap.find id env with Not_found -> raise (Error "FreeVariable"))
    | LET (id, e1, e2) -> eval((EnvMap.add id (eval(env, e1)) env), e2)
  let print_value (v : value) : unit = print_int v
end
