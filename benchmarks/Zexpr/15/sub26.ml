(* 2011-10915 / 생명과학부 / 신지민 / Homework 2-7 *)


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

   type value = int
   type environment = (id,value) Hashtbl.t

   let emptyEnv : environment = Hashtbl.create 100
   let rec eval : environment * expr -> value = fun(env,ex) ->
	match ex with
	| NUM i -> i
     	| PLUS (e1,e2) -> eval(env,e1) + eval(env,e2) 
     	| MINUS (e1,e2) -> eval(env,e1) - eval(env,e2)
     	| MULT (e1,e2) -> eval(env,e1) * eval(env,e2)
     	| DIVIDE (e1,e2) -> eval(env,e1)/eval(env,e2)
     	| MAX l -> if(l=[]) then 0 else findMax(env,l,-10000000000)
     (*	| VAR x -> try Hashtbl.find env x with  
		   Not_found -> eval(env, NUM 0)*)
		(*raise(Error "FreeVariable") *)
    	| LET (x,e1,e2) -> 
		 	  begin
			let v1 = eval(env,e1) in
			let _= Hashtbl.add env x v1 in
			eval(env,e2)
			   end
	| VAR x -> try Hashtbl.find env x with  
		   Not_found -> raise(Error "FreeVariable")
    	
   and findMax : environment * expr list * value -> value = fun(env,l,max) ->
	if(l=[]) then max
	else begin
		let hd = eval(env,List.hd(l)) in
		if(hd>max) then findMax(env,List.tl(l),hd)
		else findMax(env,List.tl(l),max)
	     end
	
   let print_value : value -> unit = fun(v) ->
		print_string(string_of_int v) 
  
 end 

(*
open Zexpr 


let _= print_value (eval (emptyEnv,LET("x",LET("x",NUM 1, PLUS(NUM 2, VAR "x")), PLUS(VAR "x", VAR "x"))))
let _= print_value (eval (emptyEnv,LET("x", MAX[NUM 1; NUM 2; NUM 3], PLUS(VAR "x", VAR "x"))))
*)

