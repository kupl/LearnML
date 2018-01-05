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
  let rec eval : (environment * expr) -> value  = fun (env, exp) ->
    match exp with
    | NUM x -> x
    | PLUS(a, b) -> (eval(env, a)) + (eval(env, b))
    | MINUS(a, b) -> (eval(env, a)) - (eval(env, b))
    | MULT(a, b) -> (eval(env, a)) * (eval(env, b))
    | DIVIDE(a, b) -> if (eval(env, b) == 0) then
                        raise (Error "Divide by zero")
                      else (eval(env, a)) / (eval(env, b))
    | VAR i -> (try (List.assoc i env) with (Not_found) -> (raise (Error "FreeVariable")))
    | LET(i, exp1, exp2) -> (eval (((i, (eval (env, exp1)))::env), exp2))
    | MAX lst -> let cmp = (fun x y ->
                            let yr = (eval (env, y)) in
                            if (x > yr) then x else yr) in
                 (match lst with
                  | [] -> 0
                  | hd::tl -> List.fold_left cmp (eval (env, hd)) lst)
  let print_value = print_int
end
