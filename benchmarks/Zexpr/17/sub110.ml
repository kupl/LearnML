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
    let rec eval = fun (env, exp) ->
      match exp with
      | NUM n -> n
      | PLUS (expr1, expr2) -> eval(env, expr1) + eval(env, expr2)    (* Is it okay??... maybe. *)
      | MINUS (expr1, expr2) -> eval(env, expr1) - eval(env, expr2)
      | MULT (expr1, expr2) -> eval(env, expr1) * eval(env, expr2)
      | DIVIDE (expr1, expr2) -> eval(env, expr1) / eval(env, expr2)
      | MAX expr_list ->
        (match expr_list with
        | [] -> 0
        | hd :: tl -> eval(env, List.fold_left (fun a b -> if eval(env, a) > eval(env, b) then a else b) (List.hd expr_list) expr_list)
        )
      | VAR i -> (try List.assoc i env with Not_found -> raise (Error "FreeVariable"))
      | LET (i, expr1, expr2) -> eval((i, eval(env, expr1)) :: env, expr2)

    let print_value = fun (v) -> print_newline (print_int v)    (* Is it okay?? *)
  end