module type ZEXPR = 
sig
    exception Error of string
    type id = string
    type expr = 
        NUM of int
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
        NUM of int
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

    let rec eval (env, e) = match e with
        NUM i -> i
        | PLUS (l, r) -> eval(env, l) + eval(env, r)
        | MINUS (l, r) -> eval(env, l) - eval(env, r)
        | MULT (l, r) -> eval(env, l) * eval(env, r)
        | DIVIDE (l, r) -> eval(env, l) / eval(env, r)
        | MAX li -> (match li with
            | [] -> 0
            | x -> 
                let calc = List.map (fun a -> eval (env, a)) x in
                List.fold_left max (List.hd calc) (List.tl calc))
        | VAR id -> (match env with
            | [] -> raise (Error "FreeVariable")
            | (i, v)::t -> 
                if id = i then v
                else eval (t, e))
        | LET (id, e1, e2) -> (
            let newEnv = (id, eval(env, e1)) :: env in
            eval (newEnv, e2))

    let print_value v = 
        print_int v
end
