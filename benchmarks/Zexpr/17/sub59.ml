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

  type environment = (id * expr) list
  type value = int

  let emptyEnv = []

(*  val eval : environment * expr -> value*)
  let rec eval ((env: environment), (expr: expr)) : value =
    match expr with
    | NUM i -> i
    | PLUS (a, b) -> eval (env, a) + eval (env, b)
    | MINUS (a, b) -> eval (env, a) - eval (env, b)
    | MULT (a, b) -> eval (env, a) * eval (env, b)
    | DIVIDE (a, b) -> eval (env, a) / eval (env, b)
    | MAX l -> (match l with
               | [] -> 0
               | [e] -> eval(env, e)
               | h::t -> if eval(env, h) > eval (env, List.hd t) then eval (env, MAX ([h]@(List.tl t)))
                         else eval (env, MAX t))
    | VAR x ->
      (let rec varCheck ((env: environment), (i: id)) : value =
        match env with
        | [] -> raise (Error "FreeVariable")
        | [(a, b)] -> if a = i then eval (env, b)
                      else raise (Error "FreeVariable")
        | (ha, hb)::t -> if ha = i then eval (env, hb)
                         else varCheck (t, i)
        in varCheck (env, x))
    | LET (var, num, e) -> eval ((var, num)::env, e)
  let print_value (e: value) : unit =
    print_endline(string_of_int e);

end
