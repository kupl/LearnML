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
        end

module Zexpr : ZEXPR =
        struct
        exception Error of string
        type id = string
        type expr = NUM of int
                |PLUS of expr * expr
                |MINUS of expr * expr
                |MULT of expr * expr
                |DIVIDE of expr * expr
                |MAX of expr list
                |VAR of id
                |LET of id * expr * expr
        type environment = ENV of (id * expr) list
        type value = int
        let emptyEnv: environment = ENV []
        let eval (ev,ex) =
        let rec solv = fun (ev,ex) ->
                match ex with
                        NUM a -> a
                        |PLUS(a,b) -> (solv (ev,a)) + (solv (ev,b))
                        |MINUS(a,b) -> (solv (ev,a)) - (solv (ev,b))
                        |MULT(a,b) -> (solv (ev,a)) * (solv (ev,b))
                        |DIVIDE(a,b) -> if (solv(ev,b)) != 0 then (solv (ev,a)) / (solv (ev,b))
                                                else raise (Error "I can't solve this problem.")
                        |MAX l -> (match l with
                                        [] -> 0
                                        |(h::[]) -> solv(ev,h)
                                        |(h::t) -> if solv(ev,h) >= solv(ev,MAX t) then solv(ev,h)
                                                        else solv(ev,MAX t))
                        |VAR i -> (match ev with
                                        ENV [] -> raise (Error "I can't solve this Problem")
                                        |ENV(h::t) -> (match h with
                                                        |(a,b) ->  if ((String.compare a i) = 0) then (solv(ENV t,b))
                                                                else solv(ENV t,VAR i)))
                        |LET (i,e1,e2) -> (match ev with
                                        ENV [] -> solv(ENV((i,e1)::[]),e2)
                                        |ENV l -> solv(ENV((i,e1)::l),e2))
        in
        let a = solv(ev,ex)     in
        let _ = print_int a     in
        a
        end

open Zexpr