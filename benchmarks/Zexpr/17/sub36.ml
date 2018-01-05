module type ZEXPR = 
    sig 
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

      val print_value : value -> unit 
    end 

module Zexpr : ZEXPR = 
    struct
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

        let rec eval ((env: environment), (exp: expr)): value = 
			match exp with
			NUM x -> x
			| PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
			| MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
			| MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
			| DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
			| MAX x ->
				let rec maxValue ((valList: value list), (res: value), (isEmpty: bool)): value = 
					match valList with
					[] -> if (isEmpty) then 0
						  else res
					| hd::tl -> if (hd > res) then (maxValue (tl, hd, false))
								else maxValue(tl, res, false)
				in
				let rec expListToValList (exprList: expr list) : value list = 
					match exprList with
					[] -> []
					| hd::tl -> (eval (env, hd)) :: (expListToValList tl)
				in
				maxValue ((expListToValList x), min_int, true)
			| VAR x -> 
				let rec findVar ((envVar: environment), (var: id)): value = 
					match envVar with
					[] -> raise (Error "FreeVariable")
					| hd::tl -> if (var = (fst hd)) then snd hd
								else findVar (tl, var)
				in
				findVar (env, x)
			| LET (varID, exp1, exp2) -> eval (((varID, eval (env, exp1))::env), exp2)

        let print_value (v: value): unit = print_int (v)
    end 
