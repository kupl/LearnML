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

      type environment = id list * int list
      type value = int

      let nth_elem l e =
        let rec f l e n = 
          match l with
          | [] -> raise (Error e)
          | hd::tl -> if hd = e then n else f tl e (n+1)
        in
        f l e 0

      let get_value (env:environment) (id':id) = 
        let id_list, val_list = env in
        let nth = nth_elem id_list id' in
        List.nth val_list nth 

      let add_env env (s,n) = 
        let id_list, val_list = env in
        (s::id_list, n::val_list)

      let emptyEnv = (([],[]):environment)

      let eval ((env:environment), (expr:expr)) = 
        let rec eval' (env,expr) = 
          let max l env = 
            let rec f l max =
              match l with
              | [] -> max
              | hd::tl -> 
                  if max < eval'(env, hd) then f tl (eval'(env,hd)) else f tl max
            in
            f l 0
          in
            match expr with
            | NUM i -> (i:value)
            | PLUS(e1,e2) -> eval'(env,e1) + eval'(env,e2)
            | MINUS(e1,e2)-> eval'(env,e1) - eval'(env,e2)
            | MULT(e1,e2) -> eval'(env,e1) * eval'(env,e2)
            | DIVIDE(e1,e2) -> eval'(env,e1) / eval'(env,e2)
            | MAX l -> max l env
            | VAR s -> get_value env s
            | LET (s,e1,e2) -> eval'(add_env env (s,(eval'(env,e1))), e2)
          in

          let v = eval'(env,expr) in
          print_int v; v

  end



