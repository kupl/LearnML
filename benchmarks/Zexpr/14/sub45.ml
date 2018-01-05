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
		val emptyEnv: environment
		val eval: environment * expr -> value
		val int_of_value : value -> int
end

module ZEXPR =
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
	type environment = (id * value) list
	let emptyEnv = []
	let int_of_value : value -> int =
		fun x -> x + 0 
	let rec eval : environment * expr -> value =
		let rec findmax = 
			fun x->
				if (List.length x) > 1 then 
					if (List.nth x 0) > (List.nth x 1) then (findmax (List.append [(List.hd x)] (List.tl (List.tl x))))
					else (findmax (List.tl x))
				else (List.hd x) in
		let rec calculator : expr -> value =
			fun a -> match a with
				| NUM a -> a
				| PLUS (a, b) -> (calculator a) + (calculator b)
				| MINUS (a, b) -> (calculator a) - (calculator b)
				| MULT (a, b) -> (calculator a) * (calculator b)
				| DIVIDE (a, b) -> (calculator a) / (calculator b)
				| MAX elst -> (findmax (List.map calculator elst))
				| _ -> raise (Error "impossible calculate") in
		let rec saveid : environment * expr -> environment =
			fun x ->
				match x with
					|(p, exp) -> 
						match exp with
							|LET (id, exp1, exp2) -> (List.append [(id, (calculator exp1))] p)
							| _ -> raise (Error "impossible service") in
		let rec tuplemap :  environment -> (environment * expr -> value) -> expr list -> value list = 
			fun e f elist -> if (List.length elist) > 0 then (List.append [(f (e, (List.hd elist)))] (tuplemap e f (List.tl elist)) )
							 else [] in

		fun a -> match a with 
			|(p, exp) -> match exp with
				| NUM a -> a
				| PLUS (a, b) -> (eval (p,a)) + (eval (p,b))
				| MINUS (a, b) -> (eval (p,a)) - (eval (p,b))
				| MULT (a, b) -> (eval (p,a)) * (eval (p,b))
				| DIVIDE (a, b) -> (eval (p,a)) / (eval (p,b))
				| MAX elst -> (findmax (tuplemap p eval elst))
				| LET (id, exp1, exp2) -> (eval ((saveid (p, exp)), exp2))
				| VAR id -> try (List.assoc id p) with 
							| _ -> raise (Error "no value is in environment")
end

