module type ZEXPR = 
  sig
    exception Error of string
    type id = string
    type expr = 
      |NUM of int 
      |PLUS of expr*expr
      |MINUS of expr*expr
      |MULT of expr*expr
      |DIVIDE of expr*expr
      |MAX of expr list
      |VAR of id
      |LET of id * expr * expr
    type environment
    type value
    val emptyEnv: environment
    val eval: environment * expr -> value
    val int_of_value: value -> int
  end

module Zexpr : ZEXPR = 
  struct
    exception Error of string
    type id = string
    type expr = 
      |NUM of int 
      |PLUS of expr*expr
      |MINUS of expr*expr
      |MULT of expr*expr
      |DIVIDE of expr*expr
      |MAX of expr list
      |VAR of id
      |LET of id * expr * expr
    type value = int
    type environment = (id * value) list
    let emptyEnv = []

    let int_of_value : value -> int = 
      fun v -> v

    let add_val : id * value * environment -> environment =
      fun (i,v,e) -> (i,v)::e

    let rec get_val : id * environment -> value = 
      fun (target_id,env) -> match env with
      | (curr_id,curr_val)::tl -> 
          if (curr_id = target_id) then curr_val
          else get_val (target_id, tl)
      | _ -> raise (Error "FreeVariable")

    let rec eval : environment * expr -> value = 
      fun (env, expr) -> match expr with 
      |NUM i -> i
      |PLUS (e1, e2) -> eval (env, e1) + eval (env, e2)
      |MINUS (e1, e2) -> eval (env, e1) - eval (env, e2)
      |MULT (e1, e2) -> eval (env, e1) * eval (env, e2)
      |DIVIDE (e1, e2) -> eval (env, e1) / eval (env, e2)
      (* Need to consider div_by_zero?*)
      |MAX el -> (match el with 
        |[] -> 0
        |hd::tl ->  List.fold_left 
          (fun prev_val curr_val -> 
            if (prev_val > curr_val) then prev_val else curr_val) 
          (eval (env, hd))
          (List.rev_map (fun e -> eval (env, e)) tl))
      |VAR v -> get_val (v, env)
      |LET (new_id, new_expr, target_expr) ->
        eval (add_val (new_id, eval(env, new_expr), env), target_expr)
  end
