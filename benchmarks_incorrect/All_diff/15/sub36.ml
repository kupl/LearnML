type aexp =
| Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (aexp,x) ->
	match aexp with
| Const n -> Const 0
| Var x -> if x=x then Const 1 else Const 0
| Power (x,n) -> if x=x then
										 if n=0 then Const 0
										 else if n=1 then Const 1
										 else if n=2 then Times [Const 2; Var x]
										 else Times [Const n; Power (x,n-1)]
						 		 else Const 0
| Times lst -> 
				(match lst with
				| [] -> Const 0
				| h::[]-> diff (h,x)
				| h::t -> match h with 
								| Const 1 -> diff (Times t,x)
								| Const n -> Times [h;diff (Times t,x)]
								|_ ->  Sum[Times (diff (h,x)::t); Times [h;diff (Times t,x)]])
| Sum lst2 -> 
				(match lst2 with
				| [] -> Const 0
			  | h::[] -> (diff(h,x))
 				| h::t -> Sum[(diff (h,x));(diff ((Sum t),x))]);;
