type aexp = Const of int
		| Var of string
		| Power of string * int
		| Times of aexp list
		| Sum of aexp list

exception InvalidArgument

let rec diff (aexp, str) =
	match aexp with
	| Const d -> Const 0
	| Var s -> if str=s then Const 1
			   else Const 0
	| Power (s, d) -> if str=s then 
							if d=0 then Const 0
							else if d=1 then Const 1
							else if d=2 then Times [Const 2;Var s]
							else Times [(Const d);Power(s, d-1)]
					  else Const 0
	| Times l -> if l=[] then raise InvalidArgument
				 else let rec diff_nth_aexp i aexp_list =
				 		if aexp_list=[] then raise InvalidArgument
				 		else if i=0 then diff ((List.hd aexp_list), str)::(List.tl aexp_list)
				 		else (List.hd aexp_list)::(diff_nth_aexp (i-1) (List.tl aexp_list))
				 	in let rec diff_times_internal n aexp_list =
				 		if n<(List.length aexp_list) then (Times (diff_nth_aexp n aexp_list))::(diff_times_internal (n+1) aexp_list)
				 		else []
				 	in Sum (diff_times_internal 0 l)
	| Sum l -> if l=[] then raise InvalidArgument
			   else let diff_internal s e =
			   			diff (e, s)
			   		in Sum (List.map (diff_internal str) l)
