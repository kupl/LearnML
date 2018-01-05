(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

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
    
    val emptyEnv : environment
    val eval : environment * expr -> value

    val int_of_value : value -> int
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

    type environment = 
	| EMPTYENV
	| ENV of id * int * environment
    type value = 	
        | NUMV of int
        | PLUSV of value * value
        | MINUSV of value * value
        | MULTV of value * value
        | DIVIDEV of value * value
        | MAXV of value list

    let emptyEnv = EMPTYENV
    let rec getvalue (key, env) = match env with
	| EMPTYENV -> raise (Error("FreeVariable"))
	| ENV (id,num,env2) ->if key=id then num else getvalue(key,env2)
    let rec int_of_value v = match v with
        | NUMV(num)->num
        | PLUSV(v1,v2)-> int_of_value(v1) + int_of_value(v2)
        | MINUSV(v1,v2)-> int_of_value(v1) - int_of_value(v2)
        | MULTV(v1,v2)-> int_of_value(v1) * int_of_value(v2)
        | DIVIDEV(v1,v2)-> int_of_value(v1) / int_of_value(v2)
        | MAXV(vlist)-> let rec getmax(v,vlist) =
                        if vlist=[] then v
                        else let v2=int_of_value(List.hd vlist) in
                        if v<v2
                        then getmax(v2,List.tl vlist)
                        else getmax(v,List.tl vlist) in
                        getmax(int_of_value(List.hd vlist),List.tl vlist)
    let rec eval (env, expr) = match expr with
	| NUM (num)-> NUMV(num)
	| PLUS(e1,e2)->PLUSV(eval(env,e1),eval(env,e2))
	| MINUS(e1,e2)->MINUSV(eval(env,e1),eval(env,e2))
	| MULT(e1,e2)->MULTV(eval(env,e1),eval(env,e2))
	| DIVIDE(e1,e2)->DIVIDEV(eval(env,e1),eval(env,e2))
	| MAX(elist)->if elist=[] then raise (Error("InvalidArgument")) 
			else if List.length elist=1 
			then eval(env,List.hd elist)
			else MAXV([eval(env,List.hd elist);
						eval(env,MAX(List.tl elist))])
	| VAR(id) -> NUMV(getvalue(id,env))
	| LET(id,e1,e2) -> let newenv= ENV(id,int_of_value(eval(env,e1)),env) in				eval(newenv,e2)
	
end

