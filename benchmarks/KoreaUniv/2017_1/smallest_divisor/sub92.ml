(* problem 2-solve*)

 let smallest_divisor : int -> int
    =fun n -> 
	let sqrt n = int_of_float (sqrt (float_of_int n)) in
	let rec temp n i = if n=1 then 1 else if i = sqrt n then n else 
               if (n mod i == 0) then i else temp n (i + 1) in
    	temp n 2;;