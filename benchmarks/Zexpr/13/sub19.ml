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
    
	let rec list_max a=
	match a with
	|[]->0
	|hd::[]->hd
	|hd1::hd2::tl-> if hd1<=hd2 then list_max (hd2::tl) else list_max (hd1::tl)

	
	let makepair a b=(a,b)
	let rec find_val env a=(*env a 받아서 expr리턴*)
	match env with
	|[]->raise (Error "FreeVariable")
	|hd::tl->if (fst hd)=a then snd hd else find_val tl a


	let add_val env a b=(*env에 a의 값을 b로 세팅 *)
	(a,b)::env

    let emptyEnv = []
    let rec eval (env, e) = 
	match e with
	|NUM a->a
	|PLUS(a,b)->(eval (env, a)) + (eval (env, b))
	|MINUS (a,b) -> (eval (env, a)) - (eval (env,b))
	|MULT (a,b)->(eval (env, a)) * (eval (env, b))
	|DIVIDE(a,b) ->(eval (env, a)) / (eval (env, b))
	|MAX a-> list_max(List.map eval (List.map (makepair env) a))
	|VAR a->  find_val env a
	|LET (a, b, c)->eval ((add_val env a (eval (env, b))), c)

    let int_of_value v = v
end

