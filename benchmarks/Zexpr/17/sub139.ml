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
 (* Implement this module *)
end
