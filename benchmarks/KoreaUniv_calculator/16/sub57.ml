
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
	;;

	let rec value: exp -> int
	  = fun expression ->
	    match expression with
	    | INT(n) -> n
	    | ADD(n1, n2) -> value(n1) + value(n2)
	    | SUB(n1, n2) -> value(n1) - value(n2)
	    | MUL(n1, n2) -> value(n1) * value(n2)
	    | DIV(n1, n2) -> value(n1) / value(n2)
	    | _ -> 0
	;;

	let rec evaluate : exp -> exp -> exp
	  = fun index expression ->
	    match expression with
	    | X -> index
	    | INT(_) -> expression
	    | ADD(n1, n2) -> ADD((evaluate index n1), (evaluate index n2))
	    | SUB(n1, n2) -> SUB((evaluate index n1), (evaluate index n2))
	    | MUL(n1, n2) -> MUL((evaluate index n1), (evaluate index n2))
	    | DIV(n1, n2) -> DIV((evaluate index n1), (evaluate index n2))
	    | SIGMA(n1, n2, f) -> 
	      if value (evaluate index n1) > value (evaluate index n2)
	      then INT(0)
	      else ADD((evaluate n1 f), (evaluate (ADD(n1, INT(1))) (SIGMA((ADD(n1, INT(1))), n2, f))))
	;;

	let calculator : exp -> int
	  = fun e ->
	    value (evaluate (INT(0)) e)
	;;