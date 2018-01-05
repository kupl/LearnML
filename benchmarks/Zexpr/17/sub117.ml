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
        val print_value : value -> unit 
    end

module Zexpr = 
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
        type environment = (string * int) list 
        type value = int
        let emptyEnv : environment = []
        let rec eval : environment * expr -> value = fun c ->
            match snd c with
            |NUM a -> a
            |PLUS (a,b) -> eval (fst c, a) + eval(fst c, b)
            |MINUS (a,b) -> eval(fst c, a) - eval(fst c, b)
            |MULT (a,b) -> eval(fst c, a) * eval(fst c, b)
            |DIVIDE (a,b) -> eval(fst c, a) / eval(fst c, b)
            |MAX a ->
                if(List.length a == 0) then 0
                else if(List.length a == 1) then eval(fst c, List.hd a)
                else if(eval(fst c, List.nth a 0) > eval(fst c, List.nth a 1)) then eval(fst c, MAX ((List.nth a 0)::(List.tl (List.tl a))))
                else eval(fst c, MAX (List.tl a))
            |VAR a ->  
                if(List.mem_assoc a (fst c)) then List.assoc a (fst c) 
                else raise(Error "FreeVariable")
            |LET (a,b,x) -> eval((a,eval(fst c, b))::fst c, x)
       let print_value : value -> unit = fun x -> print_int(x)
    end