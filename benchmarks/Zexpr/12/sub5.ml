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
        let rec calc = fun (ev,ex) ->
                match ex with
                        NUM a -> a
                        |PLUS(a,b) -> (calc (ev,a)) + (calc (ev,b))
                        |MINUS(a,b) -> (calc (ev,a)) - (calc (ev,b))
                        |MULT(a,b) -> (calc (ev,a)) * (calc (ev,b))
                        |DIVIDE(a,b) -> if (calc(ev,b)) != 0 then (calc (ev,a)) / (calc (ev,b))
                                                else raise (Error "I can't solve this problem.")
                        |MAX l -> (match l with
                                        [] -> 0
                                        |(h::[]) -> calc(ev,h)
                                        |(h::t) -> if calc(ev,h) >= calc(ev,MAX t) then calc(ev,h)
                                                        else calc(ev,MAX t))
                        |VAR i -> (match ev with
                                        ENV [] -> raise (Error "I can't solve this Problem")
                                        |ENV(h::t) -> (match h with
                                                        |(a,b) ->  if ((String.compare a i) = 0) then (calc (ENV t,b))
                                                                else calc(ENV t,VAR i)))
                        |LET (i,e1,e2) -> (match ev with
                                        ENV [] -> calc(ENV((i,e1)::[]),e2)
                                        |ENV l -> calc(ENV((i,e1)::l),e2))
        in
        let a = calc(ev,ex)     in
        let _ = print_int a     in
        a
        end

open Zexpr