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
 (* Implement this module *) 
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

  let emptyEnv : environment = []
  let rec eval ((i: environment), (ex: expr)) : value =
  	match (i, ex) with
  	| (ni, NUM n) -> n
  	| (ni, PLUS(e1, e2)) -> (eval (ni, e1)) + (eval (ni, e2))
  	| (ni, MINUS(e1, e2)) -> (eval (ni, e1)) - (eval (ni, e2))
  	| (ni, MULT(e1, e2)) -> (eval (ni, e1)) * (eval (ni, e2))
  	| (ni, DIVIDE(e1, e2)) -> (eval (ni, e1)) / (eval (ni, e2))
  	| (ni, MAX l) -> 
  		(let rec find_max (ilist: value list) (now: value) : value = 
  			match ilist with 
  			| [] -> now
  			| hd::tl -> (if (now<hd) then (find_max tl hd) else (find_max tl now))
  		in 
  		let rec exl_to_val ((el: expr list), (vl: value list)): value list = 
  			match (el, vl) with 
  			| ([], _) -> vl
  			| (hd::tl, vl) -> (exl_to_val (tl, (eval (i, hd))::vl))
  		in 
  		match l with
  		| [] -> 0 
  		| _ -> (find_max (List.tl((exl_to_val (l, [])))) (List.hd(exl_to_val (l, [])))) 
    )
 	| (ni, LET(va, e1, e2)) ->  (eval (((va, (eval (ni, e1)))::ni), e2)) 

 	| (ni, VAR va) -> 
 		(match ni with
 		| (h_id, h_int)::tl ->  		
 			(if (va = h_id) then h_int
 			else (eval (tl, ex)))
 		| emptyEnv -> raise (Error "FreeVariable"))

 	let print_value (v: value): unit = 
 		print_endline(string_of_int v) 

end