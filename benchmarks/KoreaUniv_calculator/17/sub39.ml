(* problem 5*)

type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let rec calculator : exp -> int
= fun e -> (* TODO *)
	match e with 
	| X -> raise (Failure "Error")
	| INT n -> n
	| ADD(a,b) -> ((calculator a) + (calculator b))
	| SUB(a,b) -> ((calculator a) - (calculator b))
	| MUL(a,b) -> ((calculator a) * (calculator b))
	| DIV(a,b) -> let vb = calculator b in 
				(if vb = 0 then raise (Failure "Error") 
				else ((calculator a)/vb))
	| SIGMA (e1,e2,vv) -> 
		let v1 = calculator e1 in
		let v2 = calculator e2 in
		(if v1>v2 then 0 else ((calc v1 vv) + (calculator (SIGMA (INT (v1+1), INT v2,vv) ))))

and calc ie1 vv = 
	match vv with
	| X -> ie1
	| INT n -> n
	| ADD(a,b) -> ((calc ie1 a) + (calc ie1 b))
	| SUB(a,b) -> ((calc ie1 a) - (calc ie1 b))
	| MUL(a,b) -> ((calc ie1 a) * (calc ie1 b))
	| DIV(a,b) -> let vb = calc ie1 b in 
				(if vb = 0 then raise (Failure "Error") 
				else ((calc ie1 a)/vb))
	| SIGMA(a1,a2,av) -> calculator (SIGMA(a1,a2,av))
