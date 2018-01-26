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

  let calculator exp=
  	 let rec eval exp env=
			match (exp,env) with
				|(X,[])->raise NotImplemented
				|(X,hd::tl)->hd
				|(INT i,_)->i
				|(ADD(e1,e2),env)->
					let v1 = eval e1 env in
					let v2 = eval e2 env in v1 + v2	
				|(SUB(e1,e2),env)->
					let v1 =eval e1 env in
					let v2 =eval e2 env in v1 - v2
				|(MUL(e1,e2),env)->
					let v1 =eval e1 env in
					let v2 =eval e2 env in v1 * v2 
				|(DIV(e1,e2),env)->
					let v1 =eval e1 env in
					let v2 =eval e2 env in
						if v2 = 0 then raise (Failure "Divide by 0")
						else v1 / v2
				|(SIGMA(e1,e2,e3),env)->
					let v1 = eval e1 env in
					let v2 = eval e2 env in
						if v1 > v2 then raise NotImplemented
						else if (v2-v1) < 1 then (eval e3(v1::env))
						else (eval e3(v1::env))+(eval(SIGMA(INT(v1+1),INT v2,e3)) env)
					in eval exp[] 