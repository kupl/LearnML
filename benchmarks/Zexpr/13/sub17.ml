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

type environment = (id * int) list 
type value = int 


let givfst (x,y) = x
let givsnd (x,y) = y

let rec find env a = 
	match env with
	|[] -> raise (Error "FreeVariable") 
	|hd::tl -> if (givfst hd) = a then (givsnd hd) 
		 else (find tl a)
	 
let rec findmax lst =
	match lst with
	|[] -> 0
	|hd::[] -> hd
	|hd::tl -> if hd > (findmax tl) then hd
		   else (findmax tl)


let pair a b = (a,b)

let add env a b =
(a,b)::env 

let emptyEnv = [] 
let rec eval (env, e) = 
match e with 
|NUM i->i 
|PLUS(e1,e2)->(eval (env, e1)) + (eval (env, e2)) 
|MINUS (e1,e2) -> (eval (env, e1)) - (eval (env,e2)) 
|MULT (e1,e2)->(eval (env, e1)) * (eval (env, e2)) 
|DIVIDE(e1,e2) ->(eval (env, e1)) / (eval (env, e2)) 
|MAX lst-> findmax(List.map eval (List.map (pair env) lst)) 
|VAR a-> (find env a) 
|LET (a, b, c)->eval ((add env a (eval (env, b))), c) 

let int_of_value v = v 
end 

