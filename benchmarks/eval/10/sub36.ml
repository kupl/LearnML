type expr = NUM of int
	  | PLUS of expr * expr
	  | MINUS of expr * expr
	  | MULT of expr * expr
	  | DIVIDE of expr * expr
	  | MAX of expr list ;;


let rec max lst =
	let rec maxi lst n =
		match lst with
		a::b -> if (a > n) then (maxi b a) else (maxi b n)
		|[]-> n in
	maxi lst (min_int);;




let rec eval e =
	match e with
	NUM k -> k
	|PLUS (e1,e2) -> (eval e1) + (eval e2)
	|MINUS (e1,e2) -> (eval e1) - (eval e2)
	|MULT (e1,e2) -> (eval e1) * (eval e2)
	|DIVIDE (e1,e2) -> (eval e1) / (eval e2)
	|MAX lst ->max (List.map eval lst);;
