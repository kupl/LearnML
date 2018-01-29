(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
  = fun exp -> (* raise NotImplemented *)
		match exp with
		| X -> raise NotImplemented
		|	INT(n) -> n
		|	ADD(e1,e2) -> calculator(e1) + calculator(e2)
		| SUB(e1,e2) -> calculator(e1) - calculator(e2)
		| MUL(e1,e2) -> calculator(e1) * calculator(e2)
		| DIV(e1,e2) -> calculator(e1) / calculator(e2)
		| SIGMA(e1,e2,e3) -> 
				let rec sigma : int -> int -> exp -> int
				= fun a b e ->
					let ori = e in
					let rec cal : exp -> int -> exp
					= fun e a->
						match e with
							X -> INT a
						| INT(n) -> INT(n)
						| ADD(e1,e2) -> ADD((cal e1 a), (cal e2 a))
						| SUB(e1,e2) -> SUB((cal e1 a), (cal e2 a))
						| MUL(e1,e2) -> MUL((cal e1 a), (cal e2 a))
						| DIV(e1,e2) -> DIV((cal e1 a), (cal e2 a))
						| SIGMA(e1,e2,e3) -> SIGMA((cal e1 a), (cal e2 a), (cal e3 a))
						in
					if a <= b then sigma (a+1) b ori + calculator(cal e a) else 0
				in
				sigma (calculator e1) (calculator e2) e3