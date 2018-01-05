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

module Zexpr: ZEXPR = struct
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

    type environment = (string * int) list
    type value = int
    
    let emptyEnv = []
    let eval (ev, e) =
        let rec evl (ev, e) = 
            match e with
            | NUM n -> n
            | PLUS (e1, e2)     -> evl (ev, e1) + evl (ev, e2) 
            | MINUS (e1, e2)    -> evl (ev, e1) - evl (ev, e2)
            | MULT (e1, e2)     -> evl (ev, e1) * evl (ev, e2)
            | DIVIDE (e1, e2)   -> evl (ev, e1) / evl (ev, e2)
            | MAX elst -> 
                 List.fold_right (fun e out -> let t = evl (ev, e) in
                                  if out < t then t 
                                  else out) elst 0
            | VAR id -> 
            begin
                try
                    let (vid, v) = List.find (fun (xid, xv) -> xid = id) ev in
                    v
                with Not_found -> raise (Error "FreeVariable")
            end 
            | LET (id, e1, e2) ->
            begin
                let v = evl (ev, e1) in
                evl ((id, v)::ev, e2) (* Hence v value only exists in the front - front cumulative or say, nesting *)
            end
        in
        let v = evl (ev, e) in
        v

    let int_of_value v = v 
    
end
