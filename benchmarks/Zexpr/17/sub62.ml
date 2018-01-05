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

module Zexpr : ZEXPR = struct
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
  
  let rec eval ((env: environment), (exp: expr)): value =  
    match (env, exp) with
    | (env, NUM e) -> e
    | (env, PLUS(e1, e2)) -> eval(env, e1) + eval(env, e2)
    | (env, MINUS(e1, e2)) -> eval(env, e1) - eval(env, e2)
    | (env, MULT(e1, e2)) -> eval(env, e1) * eval(env, e2)
    | (env, DIVIDE(e1, e2)) -> eval(env, e1) / eval(env, e2)
    | (env, MAX []) -> 0
    | (env, MAX (hd1::tl1)) -> (match tl1 with 
                                | [] -> eval(env, hd1)
                                | (hd2::tl2) -> eval(env, MAX(NUM (max (eval (env, hd1)) (eval (env, hd2)))::tl2)))
    | (env, VAR v) -> (match env with 
                       | [] -> raise (Error "FreeVariable")
                       | (id, e)::tl -> (if id = v then e
                                         else eval (tl, VAR v))
                       )
    | (env, LET(id, e1, e2)) -> eval(((id, (eval (env, e1)))::env), e2)

  let print_value (x: value): unit = 
    print_string ((string_of_int x) ^ "\n")

end
