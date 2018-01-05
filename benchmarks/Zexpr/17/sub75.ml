(*컴퓨터공학부 2014-16775 김민지
programming language hw 2-7*)

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

  val emptyEnv: environment
  val eval: environment * expr -> value
  val print_value : value -> unit
end

module Zexpr: ZEXPR = 
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
  type environment = (id * int) list
  type value = int
  let emptyEnv = []

  let rec eval ((env:environment), (ex:expr)) : value =
    let rec maxeval ((exlist: expr list), (env: environment)) : int = 
      match exlist with
      | h::[] -> eval(env, h)
      | h::t -> if (eval(env, h)) > (maxeval(t, env)) 
                  then (eval(env, h)) else (maxeval(t, env))
      | [] -> -1 * max_int
    in
    match ex with
    | NUM x -> x
    | PLUS(e1, e2) -> eval(env, e1) + eval(env, e2)
    | MINUS(e1, e2) -> eval(env, e1) - eval(env, e2)
    | MULT(e1, e2) -> eval(env, e1) * eval(env, e2)
    | DIVIDE(e1, e2) -> eval(env, e1) / eval(env, e2)
    | MAX el -> if el == [] then 0 else maxeval(el, env)
    | VAR i -> if ((List.mem_assoc i env) == false) then raise (Error "FreeVariable")
               else (List.assoc i env)
    | LET (i, e1, e2) -> eval(((i,(eval(env, e1)))::env), e2)

  let print_value (v:value) : unit = 
    print_endline(string_of_int(v))

end

