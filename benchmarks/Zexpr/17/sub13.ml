(*컴퓨터공학부/2011-11729/안진우/2-7*)

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
                type environment = (id * int) list
                type value = int
                val emptyEnv: environment
                val eval: environment * expr -> value
                val print_value: value -> unit
        end

module Zexpr: ZEXPR =
        struct
                exception Error of string
                type id = string
                type expr = NUM    of int
                          | PLUS   of expr * expr
                          | MINUS  of expr * expr
                          | MULT   of expr * expr
                          | DIVIDE of expr * expr
                          | MAX    of expr list
                          | VAR    of id
                          | LET    of id * expr * expr
                type environment = (id * int) list
                type value = int
                
                let (emptyEnv: environment) = []
                let rec eval ((env: environment), (exp: expr)) : value =
                        match (env, exp) with
                        | (e, NUM   (x))    -> x
                        | (e, PLUS  (x, y)) -> eval(e, x) + eval(e, y)
                        | (e, MINUS (x, y)) -> eval(e, x) - eval(e, y)
                        | (e, MULT  (x, y)) -> eval(e, x) * eval(e, y)
                        | (e, DIVIDE(x, y)) -> eval(e, x) / eval(e, y)
                        | (e, MAX   (l))    -> (
                                       match l with
                                       | [] -> 0
                                       | hd::[] -> eval(e, hd)
                                       | hd::tl -> if eval(e, hd) > eval(e, MAX(tl)) then eval(e,hd) else eval(e, MAX(tl))
                        )
                        | (e, VAR   (i))    -> if (List.exists (fun (x, _) -> x = i) e) then (List.assoc i e) else raise (Error "FreeVariable")
                        | (e, LET(i, e1, e2))-> eval((List.append ((i, eval([], e1))::[]) e), e2)
                let print_value (v: value) : unit = 
                        print_endline(string_of_int(v)) 
        end
      
