module type ZEXPR =
  sig
    exception Error of string
    type id= string
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
    val eval: environment *expr -> value
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

  type value= int
	type environment = (id, value) Hashtbl.t	
  let emptyEnv= Hashtbl.create 123456
  let rec eval= fun (env, expr)->
    match expr with
	  | NUM i -> i
  	| PLUS (e1, e2)-> eval(env, e1) + eval(env, e2) 
  	| MINUS (e1, e2)-> eval(env, e1) - eval(env, e2) 
	  | MULT (e1, e2)-> eval(env, e1) * eval(env, e2) 
	  | DIVIDE (e1, e2)->
        if (eval (env, e2)=0)
        then raise (Error "divide by zero")
        else eval(env, e1)/ eval(env, e2) 
	  | MAX el ->
       (match el with
       |[] -> 0
       |hd::tl-> 
           let ehd= (eval (env, hd)) in
           let etl= (eval (env, (MAX tl))) in
           (
             if (ehd> etl)
             then ehd
             else etl
             )
       )
       (*
           if (eval (env, hd) > (eval (env, (MAX tl)) ) )
           then (eval (env, hd))
           else (eval (env, (MAX tl))) 
          *)
	  | VAR i-> 
        (
          if (Hashtbl.length env= 0) 
          then raise (Error "No id at all")
          else Hashtbl.find env i
          )
	  | LET (i, v, eff) ->
        (
          Hashtbl.add env i (eval (env, v));
          eval (env, eff) 
          )
  end


