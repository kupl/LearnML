(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

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
    
    val emptyEnv : environment
    val eval : environment * expr -> value

    val int_of_value : value -> int
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
    type env = {
        id: string;
        value: float
    }
    type environment = env list
    type value = float
    
    let emptyEnv = []
    let rec eval (en, exp) = match exp with
                | NUM(n) -> float_of_int n
                | PLUS(e1, e2) -> eval(en, e1) +. eval(en, e2)
                | MINUS(e1, e2) -> eval(en, e1) -. eval(en, e2)
                | MULT(e1, e2) -> eval(en, e1) *. eval(en, e2)
                | DIVIDE(e1, e2) -> eval(en, e1) /. eval(en, e2)
                | MAX (l) -> let rec findMax(l, en, value) =
                                match l with
                                        |[] -> value
                                        |hd::tl -> if eval(en, hd) > value then findMax(tl, en, eval(en, hd)) else findMax(tl, en, value) in
                                let findfirst(l) =
                                        match l with
                                                |[] -> let exc = "FreeVariable" in raise (Error exc)
                                                |hd::tl -> hd in
                                let exc = "FreeVariable" in
                                if l == [] then  raise (Error exc) else
                                findMax(l, en, eval(en, findfirst(l)))
                | VAR i -> let exc = "FreeVariable" in
                        let rec findId(envi, s) =
                                match envi with
                                        |[] -> raise (Error exc)
                                        |hd::tl -> if hd.id = s then hd.value else findId(tl, s) in
                                findId(en, i)
                | LET(i,e1,e2) -> let enve = {id = i; value = eval(en, e1)} in
                                        eval([enve]@en, e2)

    let int_of_value v = int_of_float v
end

