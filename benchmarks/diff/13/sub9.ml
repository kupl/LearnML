type ae =  CONST of int
	 | VAR of string
	 | POWER of string * int
	 | TIMES of ae list
	 | SUM of ae list;;

exception InvalidArgument



let rec diff(ae_ipt,target) = 
	match ae_ipt with
	  CONST(p) -> CONST(0)
	| VAR(x) -> if target = x then CONST(1) else CONST(0)
	| POWER(x, n) -> if target = x then TIMES([CONST(n); POWER(x,n-1)]) else CONST(0)
	| SUM(lst) -> if lst = [] then raise InvalidArgument
		      else begin 
			let res = ref [] in	
			for i = 0 to (List.length(lst)-1) do
				res := List.append (!res)  [(diff((List.nth lst i), target))]
			done;
			SUM(!res)
		      end
	| TIMES(lst) -> if lst = [] then raise InvalidArgument
			else begin
			 let res = ref [] in
			 for i = 0 to (List.length(lst)-1) do
				let term = List.nth lst i in
				let temp_res = ref [] in

				for j = 0 to (List.length(lst)-1) do
					if i = j then 
						temp_res := List.append (!temp_res) [diff(term,target)]
					else 
						temp_res := List.append (!temp_res) [List.nth lst j]
				done;
				
				res := (!res) @ [TIMES((!temp_res))]
			  done;
			SUM(!res)
			end;;
	
