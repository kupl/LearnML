(*
	2006-11681 강현석
	hw2 : exercise 3
*)
exception Divided_by_Zero

type expr = NUM of int
			| PLUS of expr * expr
			| MINUS of expr * expr
			| MULT of expr * expr
			| DIVIDE of expr * expr
			| MAX of expr list

let rec eval e =
	match e with
		NUM i -> i
		| PLUS (l,r) -> (eval l) + (eval r)
		| MINUS (l,r) -> (eval l) - (eval r)
		| DIVIDE (l,r) -> 
			(
			let divide_temp = (eval r) in
			if divide_temp=0 then raise Divided_by_Zero
			else (eval l) / divide_temp)
		| MULT (l,r) -> (eval l) * (eval r)
		| MAX lst ->
			(
			let max a b = 
				let eA = (eval a) in
				let eB = (eval b) in
				if eA<eB then b else a
			in
			match lst with
			[] -> 0
			| h::t -> (eval (List.fold_left max h t)))

