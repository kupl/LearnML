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
	type environment = (id * expr) list
	let emptyEnv = []
	let int_of_value : value -> int =
		fun x -> x + 0 
	let rec eval : environment * expr -> value =
		let rec findmax = 
			fun x->
				if (List.length x) > 1 then 
					if (List.nth x 0) > (List.nth x 1) then (findmax (List.append [(List.hd x)] (List.tl (List.tl x))))
					else (findmax (List.tl x))
				else if (List.length x) == 1 then (List.hd x)
				else 0 in
		let rec calculator : expr -> expr =
			fun a -> match a with
				| NUM a -> NUM a
				| PLUS (a, b) -> PLUS ((calculator a) , (calculator b) )
				| MINUS (a, b) -> MINUS((calculator a) , (calculator b) )
				| MULT (a, b) -> MULT((calculator a) , (calculator b) )
				| DIVIDE (a, b) -> DIVIDE((calculator a) , (calculator b) )
				| MAX elst -> MAX (List.map calculator elst)
				| VAR x -> VAR x
				| _ -> raise (Error "impossible calculate") in
		let rec saveidbyexp : environment * expr -> environment =
			fun x ->
				match x with
					|(p, exp) -> 
						match exp with
							|LET (id, exp1, exp2) -> (List.append [(id, exp1)] p)
							| _ -> raise (Error "impossible service") in
		let rec realeval : environment * expr -> expr =
		fun a -> match a with
			|(p, exp) -> match exp with
				| NUM a -> NUM a
				| PLUS (a, b) -> PLUS ((realeval (p,a)) ,(realeval (p,b)))
				| MINUS (a, b) -> MINUS ((realeval (p,a)) ,(realeval (p,b)))
				| MULT (a, b) -> MULT ((realeval (p,a)) , (realeval (p,b)))
				| DIVIDE (a, b) -> DIVIDE ((realeval (p,a)) , (realeval (p,b)))
				| MAX elst -> MAX (List.map calculator elst)
				| LET (id, exp1, exp2) -> let cmo = LET(id, (calculator exp1), exp2) in
										(realeval ((saveidbyexp (p, cmo), exp2)))
				| VAR id -> (List.assoc id p) in
		

		let rec finalenv : environment * expr -> environment =
			fun x ->
				match x with
					|(p, exp) -> 
						match exp with
							|LET (id, exp1, exp2) -> (List.append [(id, exp1)] [])
							| _ -> p in

		let rec exprtovalue : expr -> environment -> value =
			fun k s-> match k with
				|NUM a -> a
				|PLUS (a, b) -> (exprtovalue a s) + (exprtovalue b s)
				|MINUS (a, b) -> (exprtovalue a s) - (exprtovalue b s)
				|MULT (a, b) -> (exprtovalue a s) * (exprtovalue b s)
				|DIVIDE (a, b) -> (exprtovalue a s) / (exprtovalue b s)
				|MAX elist -> let f = fun x -> (exprtovalue x s) in
							(findmax (List.map f elist)) 
				|VAR id -> (exprtovalue (List.assoc id s) s)
				|_ -> raise (Error "Freevariable") in 

		fun a -> (exprtovalue (realeval a) (finalenv a))
end


