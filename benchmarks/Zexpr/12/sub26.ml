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
    
    val emptyEnv: environment
    val eval: environment * expr -> value
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

    type environment = (id * expr) list
    type value = int
    
    let emptyEnv = []
    let eval (env, e) = (
      let rec evalproc (env, e) = 
        let max(a,b) = 
          if a > b then
            a
          else
            b
        in

        match e with
        | NUM(x) -> 
            x
        | PLUS(a, b) -> evalproc(env, a) + evalproc(env, b)
        | MINUS(a, b) -> evalproc(env, a) - evalproc(env, b)
        | MULT(a, b) -> evalproc(env, a) * evalproc(env, b)
        | DIVIDE(a, b) -> evalproc(env, a) / evalproc(env, b)
        | MAX(l) -> (
            match l with
            | [] -> 0
            | h::t -> max(evalproc(env, h), evalproc(env, MAX(t)))
        )
        | VAR(name) -> (
          (* env 안에서 id에 해당하는 값을 찾음 *)
          let rec get_exp(env, name) = 
            match env with
            | [] -> raise (Error "UNBOUND VARIABLE")
            | (env_name, env_exp)::t -> (
                if name = env_name then
                  env_exp
                else
                  get_exp(t, name)
            )
          in
          evalproc(env, get_exp(env, name))
        )
        | LET(id, e1, e2) -> (
          let new_env = (id, e1)::env in
          evalproc(new_env, e2)
        )
      in
      let result = evalproc(env, e) in
      print_int(result);
      result
    )

end

