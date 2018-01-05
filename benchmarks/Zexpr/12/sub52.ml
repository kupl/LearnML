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

    type environment = (id * int) list
    type value = int
    
    let emptyEnv = []
    let eval (env, e) =
        let rec eval2(env, e) = 
            match e with
            | NUM n -> n
            | PLUS(e1, e2) -> eval2(env, e1) + eval2(env, e2)
            | MINUS(e1, e2) -> eval2(env, e1) - eval2(env, e2)
            | MULT(e1, e2) -> eval2(env, e1) * eval2(env, e2)
            | DIVIDE(e1, e2) -> (
                try
                    eval2(env, e1) / eval2(env, e2)
                with Division_by_zero -> raise (Error "Divide by zero")
            )
            | MAX([]) -> 0
            | MAX(h::[]) -> eval2(env, h)
            | MAX(h::t) -> max (eval2(env, h)) (eval2(env, MAX(t)))
            | VAR(x) -> (
                try
                    let (_, v) =
                        List.find (fun e -> let (a, b) = e in a = x) env
                    in v
                with Not_found -> raise (Error ("Not found " ^ x))
            )
            | LET(id, e1, e2) -> (
                eval2((id, eval2(env, e1))::env, e2)
            )
        in
        let v = eval2(env, e)
        in print_int v; v

end

