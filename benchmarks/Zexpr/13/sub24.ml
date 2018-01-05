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
	exception FreeVariable
	exception Non
	exception Non2
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

    type environment = (id *expr) list
    type value= expr
(*		| NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr
		| MULT of expr * expr
		| DIVIDE of expr * expr
		| MAX of expr list
		| VAR of id
		| LET of id * expr * expr
  *)
    let emptyEnv = []
    let rec eval (env, e) =
		match e with
		|NUM a-> NUM a
		|PLUS (a,b)-> PLUS ((eval (env, a)),(eval (env, b)))
		|MINUS (a,b)-> MINUS ((eval (env, a)),(eval (env, b)))
		|MULT (a,b)-> MULT ((eval (env, a)),(eval (env, b)))
		|DIVIDE (a,b)-> DIVIDE ((eval (env, a)),(eval (env, b)))
		|MAX []->MAX []
		|MAX (hd::tl)->
			(
			if tl=[] then MAX [eval (env, hd)]
			else let p=eval(env, (MAX tl)) in
					match p with
					|MAX p2->MAX((eval(env,hd))::p2)
					|_-> raise Non
			)
					
		|VAR id ->
			let rec find e i=
				(match e with
				|[]-> raise FreeVariable
				|hd::tl->
					match hd with
					|(p,q)->if p=i then q
							else (find tl i)
				)
			in (find env id)
		|LET (p,q,r)->eval (((p,(eval (env, q)))::env), r)

    let rec int_of_value v =
		match v with
		|NUM a->a
		|PLUS (a,b)-> (int_of_value a)+(int_of_value b)
		|MINUS (a,b)-> (int_of_value a)-(int_of_value b)
		|MULT (a,b)-> (int_of_value a)*(int_of_value b)
		|DIVIDE (a,b)-> (int_of_value a)/(int_of_value b)
		|MAX []->0
		|MAX (hd::tl)->
			let rec cmp a tl=
				match tl with
				|[]->a
				|hd2::tl2->
					if(a<(int_of_value hd2)) then (cmp (int_of_value hd2) tl2)
					else (cmp a tl2)
			in
			(cmp (int_of_value hd) tl)
		|_->raise Non2
end





