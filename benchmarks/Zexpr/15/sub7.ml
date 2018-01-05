(* SNU Programming Language Fall 2015
 * Homework 2 
 * Exercise 7: Zexpr
 * Written by Dongho Kang 
 *)

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
;;

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
       
    type value = int    (* value of Zexpr is int type *)
    type environment = (id * value) list    (* environment is a list of pair: variable's id and its value *)

    let emptyEnv = []
    let rec eval (env, exp) = 
        match exp with 
        | NUM i -> i
        | PLUS (e1, e2)     -> eval(env, e1) + eval(env, e2)
        | MINUS (e1, e2)    -> eval(env, e1) - eval(env, e2)
        | MULT (e1, e2)     -> eval(env, e1) * eval(env, e2)
        | DIVIDE (e1, e2)   -> eval(env, e1) / eval(env, e2)
        | MAX e_list        ->
                let rec find_max (e_list, m) = 
                    (* find max from list *)
                    (* m is max item yet. it's value. *)
                    if  e_list = [] then m  (* base case *)
                    else if m > eval (env, List.hd e_list) then find_max (List.tl e_list, m)
                    else find_max (List.tl e_list, eval (env, List.hd e_list))
                in

                if e_list = [] then 0
                else find_max (e_list, eval (env, List.hd e_list))
        | VAR id            -> 
                let rec find_var: environment * id -> value = fun (env, v) ->
                    (* find variable and its value from environment *)
                    if env = [] then raise (Error "FreeVariable") 
                    else if fst (List.hd env) = v then snd (List.hd env) 
                    else find_var (List.tl env, v) (* recursive step *)
                in

                find_var (env, id)
        | LET (v, e1, e2)   ->
                eval((v, eval(env, e1))::env, e2)

    let print_value value = print_int(value)
end 
;;

