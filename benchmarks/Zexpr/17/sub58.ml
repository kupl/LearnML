(*
 *  CSE / 2013-11426 / Im DongYeop
 *  Homework 2: Exercise 7
 *)

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

	type environment = (string * int) list
	type value = int

	let rec reverselist((input: expr list), (output: expr list)): expr list =
		match input with
		| [] -> output
		| (hd::tl) -> reverselist(tl, hd::output)

	let rec findint((env: environment), (id: string)): value =
		match env with
		| [] -> raise (Error "FreeVariable")
		| (hd::tl) ->
			(match hd with
			| (h, t) -> (
				if h = id
					then t
				else
					findint(tl, id))
			| _ -> raise (Error "FreeVariable"))

(*	let addenv((id: string), (num: value), (env: environment)): environment =
	*)	

	let emptyEnv: environment = []
	let rec eval((en: environment), (ex: expr)): value =
		match ex with
		| NUM i -> i
		| PLUS (i1, i2) -> 
			(match (i1, i2) with
			| (NUM in1, NUM in2) -> in1 + in2
			| _ -> eval(en, i1) + eval(en, i2))
		| MINUS (i1, i2) ->
			(match (i1, i2) with
			| (NUM in1, NUM in2) -> in1 - in2
			| _ -> eval(en, i1) - eval(en, i2))
		| MULT (i1, i2) ->
			(match (i1, i2) with
			| (NUM in1, NUM in2) -> in1 * in2
			| _ -> eval(en, i1) * eval(en, i2))
		| DIVIDE (i1, i2) ->
			(match (i1, i2) with
			| (NUM in1, NUM in2) -> in1 / in2
			| _ -> eval(en, i1) / eval(en, i2))
		| MAX elist ->
			(match elist with
			| [] -> 0
			| (hd::tl) ->
				(match tl with
				| [] -> eval(en, hd)
				| (h::t) -> (
					if eval(en, hd) > eval(en, h)
						then eval(en, MAX([hd]@t))
					else
						eval(en, MAX(tl)))))
		| VAR id -> findint(en, id)
		| LET (id ,e1, e2) -> eval([id, eval(en, e1)]@en , e2)
		| _ -> raise (Error "Evalerror")

	let print_value(v: value): unit =
		print_int(v)
end

