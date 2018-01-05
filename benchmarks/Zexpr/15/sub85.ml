(*
  CSE/2015-21233/김종권
  Homework 2-7
*)
module type ZEXPR =
sig
  exception Error of string
  type id = string
  type expr = NUM of int
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

module Zexpr : ZEXPR =
struct
  exception Error of string
  type id = string
  type expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr
            | MULT of expr * expr
            | DIVIDE of expr * expr
            | MAX of expr list
            | VAR of id
            | LET of id * expr * expr
  type environment = (id * value) list
  and value = int
  let emptyEnv = []
                 
  let rec findEnv name e =
    match e with
    | (name', v) :: tl ->
      if name = name' then v else
      findEnv name tl
    | _ ->
      raise (Error "FreeVariable")
        
  let rec eval (env, e) =
    match e with
    | NUM i -> i
    | PLUS (e1, e2) ->
      (eval (env, e1)) + (eval (env, e2))
    | MINUS (e1, e2) ->
      (eval (env, e1)) - (eval (env, e2))
    | MULT (e1, e2) ->
      (eval (env, e1)) * (eval (env, e2))
    | DIVIDE (e1, e2) ->
      (eval (env, e1)) / (eval (env, e2))
    | MAX elist ->
      let (v, elist) =
        match elist with
        | hd :: tl -> (eval (env, hd), tl)
        | [] -> (0, [])
      in
      let fold_expr acc e =
        let v = (eval (env, e)) in
        if acc < v then v
        else acc
      in
      List.fold_left fold_expr v elist
    | VAR name ->
      (findEnv name env)
    | LET (name, e1, e2) ->
      eval ((name, (eval (env, e1))) :: env, e2)

  let print_value = print_int
end
