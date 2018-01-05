(*
	2006-11681 강현석
	hw2 : exercise 2
*)

exception Invalid_Variable
exception Divided_by_Zero
exception Invalid_Sigma_Condition


type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp *exp
		 | INTEGRAL of exp * exp * exp

let mathemadiga e =
	let rec sub_math e lst=
		match e with
		X -> (match lst with
			  [] -> raise Invalid_Variable
			  | h::t -> h)
		| INT a -> (float_of_int a)
		| REAL a -> a
		| ADD (l,r) -> (sub_math l lst) +. (sub_math r lst)
		| SUB (l,r) -> (sub_math l lst) -. (sub_math r lst)
		| MUL (l,r) -> (sub_math l lst) *. (sub_math r lst)
		| DIV (l,r) -> 
		  (match (sub_math r lst) with
		   0. -> raise Divided_by_Zero
		   | _ -> (sub_math l lst) /. (sub_math r lst))
		| SIGMA (c1,c2,e) -> 
		  (let diff = (sub_math (SUB (c2,c1)) lst) in 
		   match diff with
		   0. -> (sub_math e ((sub_math c2 lst)::lst))
		   | _ when diff>0. -> 
		     (sub_math e ((sub_math c1 lst)::lst))
			  +.(sub_math (SIGMA ((ADD (c1,INT 1)),c2,e)) lst)
		   | _ -> raise Invalid_Sigma_Condition
		  )
		| INTEGRAL (c1,c2,e) ->
		  (if (sub_math c1 lst)>(sub_math c2 lst) then 
		  		-.(sub_math (INTEGRAL (c2,c1,e)) lst)
		   else if (sub_math c1 lst)=(sub_math c2 lst) then 
			   	0.
		   else (
			   	let df = (sub_math (SUB (c2,c1)) lst) in
				if df<=0.1 then
					(sub_math e ((sub_math c1 lst)::lst)) *. df
				else 
					((sub_math e ((sub_math c1 lst)::lst)) *. 0.1) +.
					(sub_math (INTEGRAL ((ADD (c1,REAL 0.1)),c2,e)) lst)))
	in

	(sub_math e [])
			     


		
