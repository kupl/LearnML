type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list

exception InvalidArgument

let rec diff (ae, str) =
	match ae with
	| CONST d -> CONST 0
	| VAR s -> if str=s then CONST 1
			   else CONST 0
	| POWER (s, d) -> if str=s then 
							if d=0 then CONST 0
							else if d=1 then CONST 1
							else if d=2 then TIMES [CONST 2;VAR s]
							else TIMES [(CONST d);POWER(s, d-1)]
					  else CONST 0
	| TIMES l -> if l=[] then raise InvalidArgument
				 else let rec diff_nth_ae i ae_list =
				 		if ae_list=[] then raise InvalidArgument
				 		else if i=0 then diff ((List.hd ae_list), str)::(List.tl ae_list)
				 		else (List.hd ae_list)::(diff_nth_ae (i-1) (List.tl ae_list))
				 	in let rec diff_times_internal n ae_list =
				 		if n<(List.length ae_list) then (TIMES (diff_nth_ae n ae_list))::(diff_times_internal (n+1) ae_list)
				 		else []
				 	in SUM (diff_times_internal 0 l)
	| SUM l -> if l=[] then raise InvalidArgument
			   else let diff_internal s e =
			   			diff (e, s)
			   		in SUM (List.map (diff_internal str) l)
