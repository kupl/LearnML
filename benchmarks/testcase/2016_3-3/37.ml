 type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec semi_sigma : exp -> int -> int
	= fun expect n -> match expect with
		| X -> n
		| INT m -> m				
		| ADD (a,b) -> semi_sigma a n + semi_sigma b n
		| SUB (a,b) -> semi_sigma a n - semi_sigma b n
		| MUL (a,b) -> semi_sigma a n * semi_sigma b n
		| DIV (a,b) -> semi_sigma a n / semi_sigma b n

let rec sigma : int -> int -> exp -> int
  = fun n1 n2 expec -> if n1=n2 then (semi_sigma expec n1) else (semi_sigma expec n1) + sigma (n1+1) n2 expec
  	
  let rec calculator : exp -> int
  = fun exp -> match exp with
		| INT n -> n
		| ADD (n1,n2) -> calculator n1 + calculator n2
		| SUB (n1,n2) -> calculator n1 - calculator n2
		| MUL (n1,n2) -> calculator n1 * calculator n2
		| DIV (n1,n2) -> calculator n1 / calculator n2
		| SIGMA (n1,n2,f) -> sigma (calculator n1) (calculator n2) f