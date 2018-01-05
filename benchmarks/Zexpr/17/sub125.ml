(* exercise 7*)
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

module Zexpr =
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
                type environment = Env of (id * int) list
                type value = int
                let emptyEnv = Env([]) 
                let rec eval (env, elem) =
                        match (env, elem) with
                        | (_, NUM(x)) -> x 
                        | (Env(e), PLUS(y, z)) -> (eval (Env(e), y) + eval (Env(e), z))
                        | (Env(e), MINUS(y, z)) -> (eval (Env(e), y) - eval (Env(e), z))
                        | (Env(e), MULT(y, z)) -> (eval (Env(e), y) * eval (Env(e), z))
                        | (Env(e), DIVIDE(y, z)) -> (
                                if ((eval (Env(e), z))==0) then raise (Error "Division by zero")
                                else (eval (Env(e), y) / eval (Env(e), z))
                        )
                        | (Env(e), MAX(l)) -> (
                                let rec elementmax li = 
                                        match li with
                                        | [x] -> (eval (Env(e), x))
                                        | h::t -> (max (eval (Env(e), h)) (elementmax t))
                                        | [] -> 0 
                                in
                                elementmax l
                        )
                        | (Env(e), VAR(s)) -> (
                                if (List.mem_assoc s e) then (List.assoc s e)
                                else raise (Error "FreeVariable")
                        )
                        | (Env(e), LET(s, y, z)) -> eval (Env((s, eval (Env(e), y))::(List.filter (fun x -> (fst x != s)) e)), z)
                 let print_value x = 
                         print_endline (string_of_int x)
        end
