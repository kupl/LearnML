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

        type environment = (id * int) list
        type value = int

        val emptyEnv: environment
        val eval: environment * expr -> value

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

        type environment = (id * int) list
        type value = int

        let emptyEnv = []
        let eval (env, e) =
                let rec defvar (env,a) =
                        match env with
                        |hd::tl -> (
                                match hd with
                                |(b,c) -> if(a=b) then c else defvar(tl,a)
                                )
                        |[] -> raise (Error "a")
                in
                let rec calc (env,e) =
                        match e with
                        |NUM a -> a
                        |PLUS (a,b) -> (calc (env,a)) + (calc (env,b))
                        |MINUS (a,b) -> (calc (env,a)) - (calc (env,b))
                        |MULT (a,b) ->  (calc (env,a)) * (calc (env,b))
                        |DIVIDE (a,b) -> (calc (env,a)) / (calc (env,b))
                        |MAX [] -> 0
                        |MAX (hd::tl) -> if ((calc (env,hd)) > (calc (env,MAX tl))) then (calc (env,hd))
                                        else (calc (env,MAX tl))
                        |VAR a -> defvar (env,a)
                        |LET (a,b,c) -> calc ((a,(calc (env,b)))::env,c)
                in
                print_int (calc (env,e));(calc (env,e))

end
