(* 2015-1478 Giyeon Kim HW 2 *)

(* Exercise 7 *)
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
        val print_value: value -> unit
    end
module Zexpr: ZEXPR =
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
        type value = int
        type environment = (id * value) list
        let emptyEnv: environment = []
        let rec eval: environment * expr -> value = fun (ienv, iexpr) ->
            match iexpr with
            | NUM lint -> lint
            | PLUS (lexpr, rexpr) -> eval(ienv, lexpr) + eval(ienv, rexpr)
            | MINUS (lexpr, rexpr) -> eval(ienv, lexpr) - eval(ienv, rexpr)
            | MULT (lexpr, rexpr) -> eval(ienv, lexpr) * eval(ienv, rexpr)
            | DIVIDE (lexpr, rexpr) -> eval(ienv, lexpr) / eval(ienv, rexpr)
            | MAX lexprList -> 
                (match lexprList with
                | [] -> 0
                | hd::[] -> eval (ienv, hd)
                | hd::tl -> let max(x, y) = if (x > y) then x else y in
                            max((eval (ienv, hd)), (eval (ienv, MAX tl))))
            | VAR lid ->
                (try snd (List.find (fun ele -> fst ele = lid) ienv) with
                Not_found -> raise (Error "FreeVariable"))
            | LET (lid, lexpr, rexpr) -> eval((lid, eval(ienv, lexpr))::ienv, rexpr)
        let print_value = print_int
    end


