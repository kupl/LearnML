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

    type value = int
    type environment = (id * value) list
    let emptyEnv = []

    let rec _eval (env, e) = 
      match e with 
      NUM i -> i
      | PLUS (a, b) -> _eval(env, a) + _eval(env, b)
      | MINUS (a, b) -> _eval(env, a) - _eval(env, b)
      | MULT (a, b) -> _eval(env, a) * _eval(env, b)
      | DIVIDE (a, b) -> _eval(env, a) / _eval(env, b)
      | MAX lst -> (
        match lst with 
        [] -> 0
      | (ah::at) -> _eval (env, List.fold_left (fun a b -> if (_eval (env,a)) > (_eval (env,b)) then a else b) ah at)
      )
      | VAR v -> (  
          try
            let calcVal = List.assoc v env in calcVal
          with Not_found -> raise (Error "freeVariable exception")
      )
      | LET (v, e1, e2) -> _eval ((v, _eval (env, e1))::env, e2)
    
    let eval (env, e) =
      let x = _eval (env, e) in (print_int x);(print_string "\n");x

end
