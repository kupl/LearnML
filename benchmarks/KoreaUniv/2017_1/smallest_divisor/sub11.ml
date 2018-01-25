(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
	let m = sqrt(float_of_int n) in
		let x = int_of_float m in
    		let rec help n i = 
        		if i > x then n 
    			else if n mod i == 0 then i 
    			else help n (i + 1) in
    				help n 2;;