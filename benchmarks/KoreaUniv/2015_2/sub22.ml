(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
 match lst with
  |[] -> []
  |[hd] -> if (pred hd) = true then [hd] else []
  |hd::tl -> if (pred hd) = true then [hd]@(filter pred tl) else filter pred tl 
  
(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper (lst1,lst2) =
 match lst1 with
  |[] -> lst2
  |[hd] -> hd::lst2
  |hd::tl -> hd::(zipper (lst2,tl)) 
  
(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter (n,f) num =
 match n with
  | 0 -> num
  | n -> iter (n-1,f) (f num)

(*********************)					
(* Problem 4: Diff   *)
(*********************)
(* 15.10.05.01:50*)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff (aexp,str) = 
 match aexp with
  | Const n -> Const 0
  | Var x -> 
		if x = str then Const 1 
		else Const 0
  | Power (x,n) -> 
		if n = 1 then Const 1
	    else if x = str then Times [Const n;(Power (x,n-1))]
	    else Const 0
  | Times alist -> 
		(match alist with
		| [] -> raise (Failure "Empty List")
		| [hd] -> diff (hd,str)
		| hd::tl -> 
			(match hd with
			| Const 0 -> Const 0
			| Const 1 -> diff ((Times tl),str)
			| Const n -> Times [hd;diff ((Times tl),str)]
			| Var x -> Times [diff (hd,str);diff ((Times tl),str)]
			| Power (x,n) -> Times [diff ((Power (x,n)),str);diff ((Times tl),str)]
			| Times alist2 -> Sum [Times [(diff ((Times alist2),str)); (Times tl)]; Times [(Times alist2);(diff ((Times tl),str))]]
			| Sum alist2 -> Sum [Times [(diff ((Sum alist2),str)); (Times tl)]; Times [(Sum alist2);(diff ((Times tl),str))]]
			)
		)
  | Sum alist ->
		(match alist with
		| [] -> raise (Failure "Empty List")
		| [hd]	-> diff (hd,str)
		| hd::tl ->
			(match hd with
			| Const 0 -> Const 0
			| Const n -> diff ((Sum tl),str)
			| Var x -> Sum [diff ((Var x),str);diff ((Sum tl),str)]
			| Power (x,n) -> Sum [diff ((Power (x,n)),str);diff ((Sum tl),str)]
			| Times alist2 -> Sum [diff ((Times alist2),str);diff ((Sum tl),str)]
			| Sum alist2 -> Sum [diff ((Sum alist2),str);diff ((Sum tl),str)]
			)
		)

(*************************)
(* Problem 5: Calculator *)
(*************************)
(* 2015.10.06 11:50*)

type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator exp =
	match exp with
	 | X -> raise (Failure "nothing to calculate") 
	 | INT n -> n
	 | ADD (exp1,exp2) -> (calculator exp1) + (calculator exp2)
	 | SUB (exp1,exp2) -> (calculator exp1) - (calculator exp2)
	 | MUL (exp1,exp2) -> (calculator exp1) * (calculator exp2)
	 | DIV (exp1,exp2) -> if (calculator exp2) = 0 then raise (Failure "divided by zero") 
						  else (calculator exp1) / (calculator exp2)
	 | SIGMA (exp1,exp2,exp3) -> if (calculator exp1) > (calculator exp2) then raise (Failure "sigma range error") else
		(match exp3 with
		 | X -> if (calculator exp1) = (calculator exp2) then (calculator exp1)
				else (calculator (SIGMA (exp1,exp1,exp1))) + (calculator (SIGMA (INT (calculator (ADD (exp1,INT 1))),exp2,exp3)))
		 | INT n -> if (calculator exp1) = (calculator exp2) then (calculator exp3) 
					else  (calculator exp3) + (calculator (SIGMA (INT (calculator (ADD (exp1,INT 1))),exp2,exp3)))
		 | ADD (exp4,exp5) -> (calculator (SIGMA (exp1,exp2,exp4))) + (calculator (SIGMA (exp1,exp2,exp5)))
		 | SUB (exp4,exp5) -> (calculator (SIGMA (exp1,exp2,exp4))) - (calculator (SIGMA (exp1,exp2,exp5)))
		 | MUL (exp4,exp5) ->  if 	(calculator exp1) = (calculator exp2) then 
									(calculator (SIGMA (exp1,exp1,exp4))) * (calculator (SIGMA (exp1,exp1,exp5)))
							   else (calculator (SIGMA (exp1,exp1,exp4))) * (calculator (SIGMA (exp1,exp1,exp5))) +
									(calculator (SIGMA (INT (calculator (ADD (exp1,INT 1))),exp2,exp3)))
		 | DIV (exp4,exp5) -> if (calculator exp5) = 0 then raise (Failure "divided by zero")
							  else if 	(calculator exp1) = (calculator exp2) then
										(calculator (SIGMA (exp1,exp1,exp4))) / (calculator (SIGMA (exp1,exp1,exp5)))
							  else  	(calculator (SIGMA (exp1,exp1,exp4))) / (calculator (SIGMA (exp1,exp1,exp5))) +
										(calculator (SIGMA (INT (calculator (ADD (exp1,INT 1))),exp2,exp3))) 
		 
		 | SIGMA (exp4,exp5,exp6) -> if (calculator exp4) > (calculator exp5) then raise (Failure "sigma range error") else
								(calculator (SIGMA (exp4,exp5,INT (calculator exp3))))
		)
		