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

  let rec getVal (env,id) : value = match env with
    ((n,v)::tl) -> if (String.equal n id) then v else getVal(tl,id)
    |[] -> raise (Error "FreeVariable")

  let rec eval (env,exp) : value = match exp with
    | NUM v -> v
    | PLUS (e1,e2) -> eval(env,e1) + eval(env,e2)
    | MINUS (e1,e2) -> eval(env,e1) - eval(env,e2)
    | MULT (e1,e2) -> eval(env,e1) * eval(env,e2)
    | DIVIDE (e1,e2) -> eval(env,e1) / eval(env,e2)
    | MAX elist -> (match elist with
      hd::tl -> let e = eval(env,hd) in
                (match tl with
                  h::t-> let m = eval(env,MAX(tl))
                         in if (e>m) then e else m
                  |[] -> e)
      |[] -> 0)
    | VAR id -> getVal(env,id)
    | LET (id,ie,e) -> eval((id,eval(env,ie))::env,e)

  let print_value v : unit = print_int v

end
