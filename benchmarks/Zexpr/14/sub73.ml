module type ZEXPR = sig
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

  val int_of_value : value -> int
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

  let int_of_value v = v

  let rec eval = function (env, e) ->
    match e with
      | NUM n -> n
      | PLUS (x, y) -> eval(env, x) + eval(env, y)
      | MINUS (x, y) -> eval(env, x) - eval(env, y)
      | MULT (x, y) -> eval(env, x) * eval(env, x)
      | DIVIDE (x, y) -> eval(env, x) / eval(env, y)
      | MAX lst ->
          if ((List.length lst) = 0) then 0
          else if ((List.length lst) = 1) then eval(env, (List.hd lst))
          else
            let fst = eval(env, (List.hd lst)) in
            let rest = eval(env, MAX (List.tl lst)) in
            if (fst > rest) then fst else rest
      | VAR id ->
          let rec findId = function (env, id) ->
            if (env = []) then raise (Error "FreeVariable")
            else if ((fst (List.hd env)) = id) then (snd (List.hd env))
            else findId(List.tl env, id) in
          findId(env, id)
      | LET (id, x, y) -> eval((id, eval(env, x))::env, y)
end
