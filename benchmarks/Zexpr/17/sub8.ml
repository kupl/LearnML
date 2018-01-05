module type ZEXPR =
  sig
    exception Error of string
    type id = string
    type expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr
      | MULT of expr * expr | DIVIDE of expr * expr | MAX of expr list
      | VAR of id | LET of id * expr * expr

    type environment
    type value

    val emptyEnv: environment
    val eval: environment * expr -> value

    val print_value: value -> unit
  end

module Zexpr : ZEXPR =
  struct
    exception Error of string
    type id = string
    type expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr
      | MULT of expr * expr | DIVIDE of expr * expr | MAX of expr list
      | VAR of id | LET of id * expr * expr

    type value = int
    type envvar = id * value
    type environment = envvar list

    let emptyEnv = []

    let rec eval (env, expr) = match expr with
    NUM(i) -> i
    | PLUS(e1, e2) -> eval(env, e1) + eval(env, e2)
    | MINUS(e1, e2) -> eval(env, e1) - eval(env, e2)
    | MULT(e1, e2) -> eval(env, e1) * eval(env, e2)
    | DIVIDE(e1, e2) -> eval(env, e1) / eval(env, e2)
    | MAX(elist) -> ( let rec max_list l = match l with
      | [] -> 0
      | hd::tl -> ( match tl with [] -> eval(env, hd)
        | _ -> (max (eval(env,hd)) (max_list tl)) )
        in max_list elist )
    | VAR(name) -> ( let find_env ev = match ev with (id, _) -> String.equal id name
      in try let (_, v) = List.find find_env env
      in v
      with Not_found -> raise (Error "FreeVariable") )
    | LET(name, exp1, exp2) -> eval((name, eval(env, exp1))::env, exp2)

    let print_value v = print_endline (string_of_int v)
  end
