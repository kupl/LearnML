(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #7 *)
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
	
	type environment = (string * int) list
	type value = int

	let emptyEnv = []

	let rec eval(p : environment * expr) : value =
		match p with
		| (e, NUM x) -> x
		| (e, PLUS (x, y)) -> eval(e, x) + eval(e, y)
		| (e, MINUS (x, y)) -> eval(e, x) - eval(e, y)
		| (e, MULT (a, b)) -> eval(e, a) * eval(e, b)
		| (e, DIVIDE (a, b)) -> eval(e, a) / eval(e, b)
		| (e, MAX l) -> let m = List.map (fun x->eval(e,x)) l in if m=[] then 0
				else let s = ref (List.nth m 0) in for i=0 to (List.length m-1) do if (List.nth m i > !s) then s := List.nth m i else () done; !s
		| (e, VAR v) -> if(List.exists (fun x->(fst(x)=v)) e)
						then snd(List.find (fun x->(fst(x)=v)) e)
						else raise (Error "FreeVariable")
		| (e, LET(v, a, b)) -> eval((v,eval(e,a))::e, b)
		
	
	let print_value (v : value) = print_int(v)
end
