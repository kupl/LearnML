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

    type environment = (id * int ) list
    type value = int
    
    let emptyEnv = []
    let eval (env, exp) = 
		let rec subEval env exp =
            let rec calAll lst env=
                match lst with
                | [] -> []
                | hd::tl -> (let res = (subEval env hd) in res::(calAll tl env))
            in
            let rec findMax lst =
                match lst with
				| []	-> 0
                | [hd] -> hd
                | hd::tl -> (let res = (findMax tl) in if (hd >= res) then hd else res)
            in
            match exp with
            | NUM  i            -> i
            | PLUS (exp1, exp2) -> (subEval env exp1) + (subEval env exp2)
            | MINUS (exp1, exp2)-> (subEval env exp1) - (subEval env exp2)
            | MULT (exp1, exp2) -> (subEval env exp1) * (subEval env exp2)
            | DIVIDE (exp1, exp2) -> (let dvdr = (subEval env exp2)in 
									if (dvdr != 0) then ((subEval env exp1) / dvdr) 
									else raise (Error "DIVIDE_BY_ZERO"))
            | MAX lst           -> (let resLst = (calAll lst env) in (findMax resLst))
            | VAR v             -> if (List.mem_assoc v env)
                                    then (List.assoc v env)
                                    else raise (Error "CANNOT_FIND_VALUE_OF_VAL")
            | LET (id, exp1, exp2)-> (let res = (subEval env exp1) in (subEval ((id, res)::env) exp2))
        in
        (let ans = (subEval env exp) in (print_int ans);(print_newline()); ans)


    
end

    
