(* SNU Programming Language Fall 2015
 * Homework 1 
 * Exercise 4: eval
 * Written by Dongho Kang 
 *)

type formula = True 
| False
| Not       of formula
| AndAlso   of formula * formula
| OrElse    of formula * formula
| Imply     of formula * formula
| Equal      of exp * exp

and exp = Num of int
| Plus  of exp * exp
| Minus of exp * exp
;;

let rec eval: formula -> bool = fun f ->

    let rec ex_to_int: exp -> int = fun e -> (* sub-ftn for exp type *)
        match e with 
    | Num e -> e        (* if e is int *)
    | Plus (e1, e2) ->  (* if e is sum of e1 and e2 *)
            ex_to_int e1 + ex_to_int e2
    | Minus (e1, e2) -> (* if e is diff of e1 and e2 *)
            ex_to_int e1 - ex_to_int e2
    in (* change exp to int *)

    match f with 
    | True      -> true
    | False     -> false
    | Not f1   -> 
            if eval f1 = true then false    (* Not of true is false *)
            else true                       (* Not of false is true *)
    | AndAlso (f1, f2) ->
            if eval f1 = true && eval f2 = true then true   (* true, true is true *)
            else false                                      (* otherwise false *)
    | OrElse (f1, f2) ->    
            if eval f1 = false && eval f2 = false then false(* false, false is false *)
            else true                                       (* otherwise true *)
    | Imply (f1, f2) ->
            if eval f1 = false then true                    (* false, true / false is true *)
            else if eval f2 = true then true                (* true, true is true *)
            else false                                      (* true, false is false *)
    | Equal (ex1, ex2) ->                                    
            ex_to_int ex1 = ex_to_int ex2                   
;;

