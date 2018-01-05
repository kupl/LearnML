module type ZEXPR = 
	sig
		exception Error of string
		type id = string
		type expr = NUM of int
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
		type expr = NUM of int
		          | PLUS of expr * expr
							| MINUS of expr * expr
							| MULT of expr * expr
							| DIVIDE of expr * expr
							| MAX of expr list
							| VAR of id
							| LET of id * expr * expr
	 type value = int
	 type environment = (id, value) Hashtbl.t
	 let emptyEnv : environment(* (id, value) Hashtbl.t *) = Hashtbl.create 64



   let rec subEval (e, tbl, max) : value =
		

		match e with
		| NUM i -> i
		| PLUS(e1, e2) -> subEval (e1, tbl, max) + subEval (e2, tbl, max)
		| MINUS(e1, e2) -> subEval (e1, tbl, max) - subEval (e2, tbl, max)
		| MULT (e1, e2) -> subEval (e1, tbl, max) * subEval (e2, tbl, max)
		| DIVIDE(e1, e2) -> subEval (e1, tbl, max) / subEval (e2, tbl, max)
		| MAX l -> 
			(match l with
			| [] -> 0
			| hd::tl -> let y = subEval (hd, tbl, -1000000000) in
			            if List.length tl = 0 then 
										if y > max then y else max
									else if y > max then subEval (MAX tl, tbl, y) 
									else subEval (MAX tl, tbl, max))
	  | VAR x -> if Hashtbl.mem tbl x = false then raise (Error "unboundvariable")
		           else (*let y = *)Hashtbl.find tbl x (* in Hashtbl.remove tbl x; y*)
		| LET(x, e1, e2) -> let y = subEval (e1, tbl, max) in
		                  
											(*  print_int(y);
											  print_endline "";*)
		                    Hashtbl.add tbl x y;
											(*	if Hashtbl.mem tbl x = true then print_string "added-----"
												else print_string "notAdded!!";
												print_endline "";*)
												subEval (e2, tbl, max)
		
		let eval : environment * expr -> value = fun (env, e) ->
			subEval (e, env, -1000000000)
			
		let print_value (v : value) : unit =
			let s = string_of_int v in
			print_string (s);
	end
	 