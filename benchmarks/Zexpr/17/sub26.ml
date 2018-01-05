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

module SM = Map.Make(String)

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

  type environment = int SM.t
  type value = int

  let emptyEnv = SM.empty

  let rec eval(env, expr) = match expr with
  | NUM a -> a
  | PLUS(a, b) -> eval(env, a) + eval(env, b)
  | MINUS(a, b) -> eval(env, a) - eval(env, b)
  | MULT(a, b) -> eval(env, a) * eval(env, b)
  | DIVIDE(a, b) -> eval(env, a) / eval(env, b)
  | MAX [] -> 0
  | MAX(a::[]) -> eval(env, a)
  | MAX(a::b::l) -> max (eval(env, a)) (eval(env, MAX(b::l)))
  | VAR i -> (try SM.find i env; with Not_found -> raise(Error "FreeVariable"))
  | LET(i, a, b) -> eval(SM.add i (eval(env, a)) (SM.remove i env), b)

  let print_value v = print_endline(string_of_int v)

end