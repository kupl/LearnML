(* SNU Programming Language Fall 2015
 * Homework 1 
 * Exercise 4: eval
 * Written by Dongho Kang 
 *)

type formula = TRUE 
| FALSE
| NOT       of formula
| ANDALSO   of formula * formula
| ORELSE    of formula * formula
| IMPLY     of formula * formula
| LESS      of expr * expr

and expr = NUM of int
| PLUS  of expr * expr
| MINUS of expr * expr
;;

let rec eval: formula -> bool = fun f ->

    let rec ex_to_int: expr -> int = fun e -> (* sub-ftn for expr type *)
        match e with 
    | NUM e -> e        (* if e is int *)
    | PLUS (e1, e2) ->  (* if e is sum of e1 and e2 *)
            ex_to_int e1 + ex_to_int e2
    | MINUS (e1, e2) -> (* if e is diff of e1 and e2 *)
            ex_to_int e1 - ex_to_int e2
    in (* change expr to int *)

    match f with 
    | TRUE      -> true
    | FALSE     -> false
    | NOT f1   -> 
            if eval f1 = true then false    (* NOT of true is false *)
            else true                       (* NOT of false is true *)
    | ANDALSO (f1, f2) ->
            if eval f1 = true && eval f2 = true then true   (* true, true is true *)
            else false                                      (* otherwise false *)
    | ORELSE (f1, f2) ->    
            if eval f1 = false && eval f2 = false then false(* false, false is false *)
            else true                                       (* otherwise true *)
    | IMPLY (f1, f2) ->
            if eval f1 = false then true                    (* false, true / false is true *)
            else if eval f2 = true then true                (* true, true is true *)
            else false                                      (* true, false is false *)
    | LESS (ex1, ex2) ->                                    
            ex_to_int ex1 < ex_to_int ex2                   
;;

