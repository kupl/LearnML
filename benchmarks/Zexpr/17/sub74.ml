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
                val emptyEnv: environment
                val eval: environment * expr -> value

                val print_value : value -> unit
        end;;
  
module Zexpr : ZEXPR = 
        struct
                exception Error of string
                type value = int
                type id = string
                type environment = ((id * value) list)
                type expr = 
                        | NUM of int
                        | PLUS of expr * expr
                        | MINUS of expr * expr
                        | MULT of expr * expr
                        | DIVIDE of expr * expr
                        | MAX of expr list
                        | VAR of id
                        | LET of id * expr * expr

                let emptyEnv = []
                let rec eval ((env : environment), (ex : expr)) : value = 
                        match ex with
                        | NUM i -> i
                        | PLUS (ex1, ex2) -> (eval (env, ex1)) + (eval (env, ex2))
                        | MINUS (ex1, ex2) -> (eval (env, ex1)) - (eval (env, ex2))
                        | MULT (ex1, ex2) -> (eval (env, ex1)) * (eval (env, ex2))
                        | DIVIDE (ex1, ex2) -> (eval (env, ex1)) / (eval (env, ex2))
                        | MAX [] -> 0
                        | MAX exList -> (let exVal = List.map (fun x -> eval(env, x)) exList in
                                List.hd(List.sort (fun x y -> y-x) exVal))
                        | VAR i -> (
                                try snd (List.find (fun x -> (fst x)=i) env) with
                                Not_found -> raise (Error "FreeVariable" ))
                        | LET (i, ex1, ex2) -> (
                                let newEnv = (i, eval(env, ex1))::(List.remove_assoc i env) in
                                eval(newEnv, ex2))               
                                
                 let print_value v = print_int (v)
        end;;

