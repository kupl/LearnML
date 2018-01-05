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

        val print_value : value -> unit 
    end

module Zexpr : ZEXPR =
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
        type environment = (id * int) list
        type value = int
        let emptyEnv = []

        let rec eval (en,ex) =
            try(

            match ex with
            NUM i -> i
            | PLUS (ex1,ex2) -> eval(en,ex1) + eval(en,ex2)
            | MINUS (ex1,ex2) -> eval(en,ex1) - eval(en,ex2)
            | MULT (ex1,ex2) -> eval(en,ex1) * eval(en,ex2)
            | DIVIDE (ex1,ex2) -> eval(en,ex1) / eval(en,ex2)
            | MAX exli -> if exli = [] then 0
                        else (maximum en exli (-1000))
            | VAR x -> List.assoc x en
            | LET (i,ex1,ex2) -> eval((i,eval (en,ex1))::en,ex2)

            ) with Not_found -> raise (Error "FreeVariable")

        and maximum en arg max =
            match arg with
            [] -> max
            | hd::tl -> if eval(en,hd) > max then maximum en tl (eval(en,hd)) else maximum en tl max

        let print_value v =
            print_int v
    end

