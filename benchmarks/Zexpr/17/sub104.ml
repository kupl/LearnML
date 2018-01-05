(*Computer Science Engineering 2015-12683 Kim Jaein*)
module type ZEXPR =
sig
	exception Error of string
	type id = string
	type expr = 
		|NUM of int
		|PLUS of expr * expr
		|MINUS of expr * expr
		|MULT of expr * expr
		|DIVIDE of expr * expr
		|MAX of expr list
		|VAR of id
		|LET of id * expr * expr

	type environment
	type value

	val emptyEnv: environment
	val eval: environment * expr -> value

	val print_value : value -> unit
end

module Zexpr : ZEXPR =
struct
	exception Error of string
	type id = string
	type expr = 
		|NUM of int
		|PLUS of expr * expr
		|MINUS of expr * expr
		|MULT of expr * expr
		|DIVIDE of expr * expr
		|MAX of expr list
		|VAR of id
		|LET of id * expr * expr

	type environment = bool
	type value = string list

	let emptyEnv = true

	let strchanger ((var:id), (e1:value), (e2:string)) =
		if e2 = var then e1 else [e2;]

	let rec eval ((env:environment), (ex:expr)) =
		match env with
		|false -> raise (Error "Environment Error")
		|true ->
			(match ex with
			|NUM i -> [string_of_int i;]
			|PLUS (i,j) -> eval (env,i) @ eval (env,j) @ ["+";]
			|MINUS (i,j) -> eval (env,i) @ eval (env,j) @ ["-";]
			|MULT (i,j) -> eval (env,i) @ eval (env,j) @ ["*";]
			|DIVIDE (i,j) -> eval (env,i) @ eval (env,j) @ ["/";]
			|MAX elist ->
				(if List.length elist = 0
				then ["0";]
				else List.concat (List.map (fun xp->eval (env, xp)) elist) @ [string_of_int (List.length elist);] @ ["max";])
			|VAR x -> [x;]
			|LET (var,e1,e2) -> List.concat (List.map (fun str->strchanger (var,(eval (env,e1)),str)) (eval (env, e2))))
	
	let rec sub_help h t l =
		match h with
		|0 ->
			(match t with
			|0 -> l
			|_ -> sub_help h (t-1) (List.rev (List.tl (List.rev l)))
			)
		|_ -> sub_help (h-1) t (List.tl l)

	let sublist s e l =
		if e < s || s < 0 || e < 0
		then []
		else sub_help s ((List.length l)-1-e) l

	let rec print_help ((stack:value), (idx:int)) =
		let len = List.length stack in
		if len = 1 
		then int_of_string (List.nth stack 0)
		else(
		let elem = List.nth stack idx in
		match elem with
		|"+" ->
			(let x = int_of_string (List.nth stack (idx-2)) in
			 let y = int_of_string (List.nth stack (idx-1)) in
			 let result = string_of_int (x + y) in
			 let newstack = (sublist 0 (idx-3) stack) @ [result;] @ (sublist (idx+1) (len-1) stack) in
			 print_help (newstack, (idx-2)))
		|"-" ->
			(let x = int_of_string (List.nth stack (idx-2)) in
			 let y = int_of_string (List.nth stack (idx-1)) in
			 let result = string_of_int (x - y) in
			 let newstack = (sublist 0 (idx-3) stack) @ [result;] @ (sublist (idx+1) (len-1) stack) in
			 print_help (newstack, (idx-2)))
		|"*" ->
			(let x = int_of_string (List.nth stack (idx-2)) in
			 let y = int_of_string (List.nth stack (idx-1)) in
			 let result = string_of_int (x * y) in
			 let newstack = (sublist 0 (idx-3) stack) @ [result;] @ (sublist (idx+1) (len-1) stack) in
			 print_help (newstack, (idx-2)))
		|"/" ->
			(let x = int_of_string (List.nth stack (idx-2)) in
			 let y = int_of_string (List.nth stack (idx-1)) in
			 let result = string_of_int (x / y) in
			 let newstack = (sublist 0 (idx-3) stack) @ [result;] @ (sublist (idx+1) (len-1) stack) in
			 print_help (newstack, (idx-2)))
		|"max" ->
			(let num = int_of_string (List.nth stack (idx-1)) in
			 let maxlist = sublist (idx-1-num) (idx-2) stack in
			 let result = List.fold_left (fun x y -> string_of_int (max (int_of_string x) (int_of_string y))) (List.hd maxlist) maxlist in
			 let newstack = (sublist 0 (idx-num-2) stack) @ [result;] @ (sublist (idx+1) (len-1) stack) in
			 print_help (newstack, (idx-num-1)))
		| _ ->
			(try print_help (stack, (idx+(int_of_string elem)-(int_of_string elem)+1))
			 with _ -> raise (Error "FreeVariable"))
		)

	let print_value (input:value) =
		print_endline (string_of_int (print_help (input, 0)))
end

