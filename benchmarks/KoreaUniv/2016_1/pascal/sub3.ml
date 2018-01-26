(* Problem 1 *)
exception Integer_overflow_problem

let round : float -> int
= fun f ->
	int_of_float (floor (f +. 0.5))

let rec calc_pascal_row : int * int * int -> float
= fun (n1, n2, i) ->
	if n2=i then
		1.
	else
		calc_pascal_row (n1, n2, i+1)*.(float_of_int (n1-i))/.(float_of_int (i+1))

let rec pascal : int * int -> int
= fun (n1, n2) -> 
	if n2>n1 || n1<0 || n2<0 then
		0
	else
		if n1=n2 || n2=0 then
			1
		else
			if n1/2<n2 then
				if int_of_float (calc_pascal_row (n1, n1-n2, 0))=0 then
					raise Integer_overflow_problem
				else
					round(calc_pascal_row (n1, n1-n2, 0))
			else
				if int_of_float (calc_pascal_row (n1, n2, 0))=0 then
					raise Integer_overflow_problem
				else
					round(calc_pascal_row (n1, n2, 0))
