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
        val emptyEnv : environment
        val eval : environment * expr -> value
        val int_of_value : value -> int
    end
;;

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
        type environment = (id * int) list
        type value = 
            int
        let emptyEnv = []
        let int_of_value(value) = value
        let rec eval(envi, expre) = 
            match expre with
            NUM n -> n
            | PLUS(n, m) -> eval(envi, n) + eval(envi, m)
            | MINUS(n, m) -> eval(envi, n) - eval(envi, m)
            | MULT(n, m) -> eval(envi, n) * eval(envi, m)
            | DIVIDE(n, m) -> eval(envi, n) / eval(envi, m)
            | MAX(elist) -> (*find max. if occur freevariable, it catched by VAR(id)*)
                    if List.length(elist) = 0 then 0
                    else if List.length(elist) = 1 then eval(envi, List.hd(elist))
                        else 
                        let fir = eval(envi, List.hd(elist)) in
                        let las = eval(envi, MAX(List.tl(elist))) in
                        if (fir >= las) then fir else las
            | VAR(id) -> (* if defined then replace, else FreeVariable*) 
                    if List.length(envi) = 0 then raise (Error "FreeVariable")
                    else if fst(List.hd(envi)) = id then snd(List.hd(envi))
                    else eval(List.tl(envi), expre)
            | LET(id, expr1, expr2) ->
                    eval((id, eval(envi, expr1))::envi, expr2) (*we have to check that this id has correct value*)
    end
;;

(*ZEXPR.eval(ZEXPR.emptyEnv, E) prints value or exception*)
(*VAR "x" : value, named x*)
(*LET("x", e1, e2) : limits scope, define name*)
(*MAX(Num list) = maximum of list | 0 if list is empty*)
