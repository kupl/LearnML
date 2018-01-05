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

    type environment = ENV of id list*int list
	type value = int
	let emptyEnv = ENV([],[])
	
    let eval(env, exp)=
     let rec evaluation(env, exp)=
      match (env,exp) with
	  |(_,NUM n)->n
      |(_,PLUS(e1,e2))->evaluation(env,e1)+evaluation(env,e2)
      |(_,MINUS(e1,e2))->evaluation(env,e1)-evaluation(env,e2)
      |(_,MULT(e1,e2))->evaluation(env,e1)*evaluation(env,e2)
      |(_,DIVIDE(e1,e2))->
	    let divisor = evaluation(env,e2)in
	    if divisor!=0 then evaluation(env,e1)/divisor
	    else raise (Error "div by Zero")
	  |(_,MAX [])->0
	  |(_,MAX (hd::tl))->
	    if tl=[] then evaluation(env,hd)
        else
	     let n1=evaluation(env,hd)in
	     let n2=evaluation(env,(MAX tl))in
	     if n1>n2 then n1
	     else n2
	  |(ENV([],_),VAR str)->raise(Error ("Undefined Variable "^str))
	  |(ENV(_,[]),VAR str)->raise(Error ("Undefined Variable "^str))
      |(ENV(sh::st,vh::vt),VAR str)->
	     if sh=str then vh
		 else evaluation(ENV(st,vt),exp)
      |(ENV(sl,vl), LET(str,e1,e2))->
        let val1 = evaluation(env, e1)in
        let newenv = ENV((str::sl),(val1::vl))in
        evaluation(newenv,e2) 
     in
	  
	  let res = evaluation(env,exp)in
	  let _ = print_int res in
	res
end

open Zexpr
