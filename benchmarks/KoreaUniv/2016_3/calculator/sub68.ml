
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let calculator : exp -> int
  = fun exp ->	let rec f t = (match t with X -> raise (Failure "impossible") |
				INT n1 -> n1 |
				ADD (n1, n2) -> (f n1) + (f n2) |
				SUB (n1, n2) -> (f n1) - (f n2) |
				MUL (n1, n2) -> (f n1) * (f n2) |
				DIV (n1, n2) -> (f n1) / (f n2) |
				SIGMA (n1, n2, n3) ->
					let rec cal k = (match k with X -> (fun x -> x) |
					INT i -> (fun x->i) |
					ADD (p, q) -> (fun x -> ((cal p) x)+((cal q) x)) |
					SUB (p, q) -> (fun x -> ((cal p) x)-((cal q) x)) |
					MUL (p, q) -> (fun x -> ((cal p) x)*((cal q) x)) |
					DIV (p, q) -> (fun x -> ((cal p) x)/((cal q) x)) |
					SIGMA (p, q, r) -> raise (Failure "impossible")) in
			let rec g v1 v2 = if v1 > v2 then 0 else if v1=v2 then ((cal n3) v1) else (((cal n3) v1) + (g (v1 + 1) v2)) in (g (f n1) (f n2))) in
			match exp with X -> 0 | 
			INT a -> a |
			ADD (a, b) -> (f (ADD (a, b))) |
			SUB (a, b) -> (f (SUB (a, b))) |
			MUL (a, b) -> (f (MUL (a, b))) |
			DIV (a, b) -> (f (DIV (a, b))) |
			SIGMA (a, b, c) -> (f (SIGMA (a, b, c)));;