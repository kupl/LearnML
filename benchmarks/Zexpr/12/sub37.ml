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
    type value = int
    type environment = (id * value) list
    
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

    type value = int
    type environment = (id * value) list
    
    let emptyEnv = []
	

    let rec eval (firstenv, firste) =

(*	let rec maxx (li,m) =
        match li with
	| [] -> m
        | (NUM hd)::tl -> if hd>m then (maxx (tl,hd)) else (maxx (tl, m))
        in
*)	
	let rec find i en=
		match en with
		| [] -> raise (Error "NoMatch")
		| (a,b)::tl -> if a=i then b else (find i tl)
	in

	let rec eval2 (env , e) =
		let rec maxx (li,m) =
		 match li with
	       	 | [] -> m
		 | hd::tl -> if (eval2 (env,hd))>m then (maxx (tl, (eval2(env,hd)))) else (maxx (tl, m))
(*	       	 | (NUM hd)::tl -> if hd>m then (maxx (tl,hd)) else (maxx (tl, m))*)
	       	 in

	 match e with
	 | NUM n -> n
       	 | PLUS (a,b) -> eval2(env, a) + eval2(env, b)
       	 | MINUS (a,b) -> eval2(env, a) - eval2(env, b)
       	 | MULT (a,b) -> eval2(env, a) * eval2(env, b)
       	 | DIVIDE (a,b) -> if (eval2(env,b))=0 then 
			raise (Error "DIVIDE_0") 
			else eval2(env, a) / eval2(env, b)
         | MAX l -> if (maxx (l, -999999))=(-999999) 
			then 0 else (maxx (l, -999999))
         | VAR i -> (find i env)
         | LET (i,e1,e2) -> eval2((i,(eval2(env,e1)))::env, e2)
	in
 
	print_int(eval2(firstenv,firste));(eval2(firstenv,firste))

	end  
