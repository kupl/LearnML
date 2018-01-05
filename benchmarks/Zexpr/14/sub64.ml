module type ZEXPR = sig
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

    val int_of_value : value -> int
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

    type environment = (string * (int list)) list
    type value = int
    
    let emptyEnv = [];;
    let int_of_value v = v;;
    
    type token =
        DEFINED of int
        | UNDEFINED

    let rec get_token env var_name = match env with
        [] -> UNDEFINED
        | (str,[])::tl ->
                if str=var_name then UNDEFINED
                else get_token tl var_name
        | (str,var_list)::tl ->
                if str=var_name then DEFINED(List.hd var_list)
                else get_token tl var_name
    ;;

    let part_insert var var_name (env_str, env_var_list) =
        if(env_str=var_name) then (env_str,var::env_var_list)
        else (env_str,env_var_list)
    ;;

    let insert var var_name env =
        let checker = get_token env var_name in
        match checker with
            UNDEFINED -> (var_name,[var])::env
            | DEFINED(_) -> List.rev_map (part_insert var var_name) env
    ;;

    (*let part_pop var_name (env_str, env_var_list) =
        if(env_str = var_name) then
            match env_var_list with hd::tl -> (env_str,tl)
        else (env_str,env_var_list)
    ;;

    let pop var_name env =
        List.rev_map (part_pop var_name) env
    ;;*)

    let rec get_max l m = match l with
        [] -> m
        | hd::tl -> get_max tl (max m hd)
    ;;

    let get_max_wrap l = match l with
        [] -> 0
        | hd::tl -> get_max tl hd

    let rec eval (env,e) = match e with
        NUM(n) -> n
        | PLUS(e1,e2) -> eval(env,e1) + eval(env,e2)
        | MINUS(e1,e2) -> eval(env,e1) - eval(env,e2)
        | MULT(e1,e2) -> eval(env,e1) * eval(env,e2)
        | DIVIDE(e1,e2) -> eval(env,e1) / eval(env,e2)
        | MAX(el) ->
            let my_eval my_env my_e = eval(my_env,my_e) in
            get_max_wrap (List.rev_map (my_eval env) el)
        | VAR(str) ->
            let tk = get_token env str in
            begin
                match tk with
                    UNDEFINED -> raise (Error "FreeVariable")
                    | DEFINED(d) -> d
            end
        | LET(str,e1,e2) ->
            let v = eval(env,e1) in
            let new_env = insert v str env in
            eval(new_env,e2)
    ;;
end
