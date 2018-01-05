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

  type environment = int
  type value = int

  let emptyEnv = 0
  let rec eval (env, exp) =
	let rec hasVar exp = 
		match exp with
		| NUM n -> false
		| PLUS (e1, e2) -> (hasVar e1) || (hasVar e2)
		| MINUS (e1, e2) -> (hasVar e1) || (hasVar e2)
		| MULT (e1, e2) -> (hasVar e1) || (hasVar e2)
		| DIVIDE (e1, e2) -> (hasVar e1) || (hasVar e2)
		| MAX e_list -> (match e_list with
				| [] -> false
				| [a] -> hasVar a
				| hd::tl -> (hasVar hd) || (hasVar (MAX tl)))
		| VAR i -> true
		| LET (i, e1, e2) -> (hasVar e1) in	(* in e2 VAR has to appear ALWAYS! *)
	let rec bigger_num (a, b) = 
		match (a, b) with
		| (NUM an, NUM bn) -> if an > bn then a else b
		| _ -> raise (Error "wrong compare") in
	let rec list_max l = 
		match l with
		| [] -> raise (Error "empty list")
		| [a] -> a
		| hd::tl -> (bigger_num (hd, (list_max tl))) in
	let rec list_fun (l, f, c, v) =
		match l with
		| [] -> []
		| hd::tl -> List.append [f (hd, c, v)] (list_fun (tl, f, c, v)) in
	(* input 'v' into exp (v: expr) *)
	let rec input_value (exp, c, v) =
		match exp with
		| NUM n -> (NUM n)
		| PLUS (e1, e2) -> PLUS ((input_value (e1, c, v)), (input_value (e2, c, v)))
		| MINUS (e1, e2) -> MINUS ((input_value (e1, c, v)), (input_value (e2, c, v)))
		| MULT (e1, e2) -> MULT ((input_value (e1, c, v)), (input_value (e2, c, v)))
		| DIVIDE (e1, e2) -> DIVIDE ((input_value (e1, c, v)), (input_value (e2, c, v)))
		| MAX e_list -> (match e_list with
				| [] -> (NUM 0)
				| l -> (list_max (list_fun (l, input_value, c, v))))
		| VAR i -> if i = c then v else (VAR i)
		| LET (i, e1, e2) -> (input_value (e2, i, input_value (e1, c, v))) in 
	(*
	let rec input_value (exp, c, v) =
		match exp with
		| NUM n -> n
		| PLUS (e1, e2) -> (input_value (e1, c, v)) + (input_value (e2, c, v))
		| MINUS (e1, e2) -> (input_value (e1, c, v)) - (input_value (e2, c, v))
		| MULT (e1, e2) -> (input_value (e1, c, v)) * (input_value (e2, c, v))
		| DIVIDE (e1, e2) -> (input_value (e1, c, v)) / (input_value (e2, c, v))
		| MAX e_list -> match e_list with
				| [] -> 0
				| l -> list_max (list_fun (l, f, c, v))
		| VAR i -> if i = c then v else (VAR i)
		| LET (i, e1, e2) -> input_value (e2, (input_value (e1, v)))
	*)
	match exp with
	| NUM n -> n
	| PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
	| MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
	| MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
	| DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
	| MAX e_list -> (match e_list with
			| [] -> 0
			| [a] -> (eval (env, a))
			| hd::tl -> max (eval (env, hd)) (eval (env, (MAX tl))))
	| VAR i -> raise (Error "empty variable")
	| LET (i, e1, e2) -> (eval (env, (input_value (e2, i, NUM (eval (env, e1))))))

  let print_value v =
	(print_endline (string_of_int v))
end

let _ = Zexpr.print_value (Zexpr.eval (Zexpr.emptyEnv, Zexpr.LET("x", NUM 1,
                   PLUS (LET("x", NUM 2, PLUS(VAR "x", VAR "x")),
VAR "x")
)
))
