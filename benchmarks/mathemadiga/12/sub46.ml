exception FreeVariable

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

let rec mathemadiga =
	let rec inmath =
		(fun (x, y) -> match x with
					| INT a -> (float)a
					| REAL a -> a
					| X -> (match y with
								| h:: t -> h
								| _ -> raise FreeVariable)
					| ADD (a, b) -> (inmath (a, y)) +. (inmath (b, y))
					| SUB (a, b) -> (inmath (a, y)) -. (inmath (b, y))
					| MUL (a, b) -> (inmath (a, y)) *. (inmath (b, y))
					| DIV (a, b) -> (inmath (a, y)) /. (inmath (b, y))
					| SIGMA (a, b, c) -> let a = int_of_float(inmath (a, y)) in
							let b = int_of_float(inmath (b, y)) in
							let rec sigma = (fun (a, b) ->
											if a > b then 0.
											else if a = b then (inmath (c, (float)a:: y))
											else (inmath (c, (float)a:: y)) +. (sigma (a +1, b))) in
							(sigma (a, b))
					| INTEGRAL (a, b, c) -> let a = (inmath (a, y)) in
							let b = (inmath (b, y)) in
							let rec inte = (fun (a, b) ->
											if (b -.a < 0.1) then (inmath (c, a:: y)) *. (b -.a)
											else (inmath (c, a:: y)) *. 0.1 +. (inte (a +.0.1, b))) in
							if (b < a) then -.(inte (b, a))
							else (inte (a, b))
			
		) in
	(fun x -> inmath (x, []));;

