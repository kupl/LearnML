(*
 * Programming Languages, 2013 Fall.
 * Skeleton Code for Exercise 2-4 -- answer.ml
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 *)

 (* KIHWAN KANG HW02-4 *)

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

	(* ENVIRONMENT IS A LIST OF PAIRS OF STRING AND PRE-DEFINED EXPRESSIONS *)
    type environment = (string * int) list
	(* VALUE *)
    type value = VAL of int
    
    let emptyEnv = []
    let eval (env, e) = 
		let rec evalrec (env, e) =
		match e with
		|NUM no -> VAL no
		|PLUS (fst, snd) -> 
			let (VAL fst_v) = evalrec (env, fst) in
			let (VAL snd_v) = evalrec (env, snd) in
			VAL (fst_v + snd_v)
		|MINUS (fst, snd) ->
			let (VAL fst_v) = evalrec (env, fst) in
			let (VAL snd_v) = evalrec (env, snd) in
			VAL (fst_v - snd_v)
		|MULT (fst, snd) ->
			let (VAL fst_v) = evalrec (env, fst) in
			let (VAL snd_v) = evalrec (env, snd) in
			VAL (fst_v * snd_v)
		|DIVIDE (fst, snd) ->
			let (VAL fst_v) = evalrec (env, fst) in
			let (VAL snd_v) = evalrec (env, snd) in
			if snd_v = 0
			then raise (Error "Divisiony zero")
			else VAL (fst_v / snd_v)
		|MAX elist ->
			(
			match elist with
			|[] -> (VAL 0)
			|[elem] -> evalrec (env, elem)
			|fst::snd -> 
				let (VAL fst_v) = evalrec (env, fst) in
				let (VAL snd_v) = evalrec (env, (List.hd snd)) in
				if fst_v < snd_v
				then evalrec (env, MAX ([(NUM snd_v)]@(List.tl snd)))
				else evalrec (env, MAX ([(NUM fst_v)]@(List.tl snd)))
			)
		|VAR vid ->  
			let rec searchEnv (env, vid) = 
				(
				match env with
				|[] -> raise (Error "FreeVariable")
				|(hvid, hval)::tl -> 
					if hvid = vid
					then (VAL hval)
					else searchEnv(tl, vid)
				) 
		in
			searchEnv (env, vid)
		|LET (vid, vexpr, nexpr) ->
			let (VAL vexpr_v) = evalrec (env, vexpr) in
			evalrec ((vid, vexpr_v)::env, nexpr)
	in
		evalrec (env, e)

	let int_of_value v = 
		match v with
		|VAL value -> value

end

