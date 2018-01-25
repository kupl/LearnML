(*problem 2*)

let smallest_divisor : int -> int 
= fun n ->
   if n < 2 then raise (Failure "the value has to be larger than 1")
   (* We have to exclude the case when n is smaller than 2 because the condition of the problem said we only consider the case when n is greater than 1*)    
   else
       if n mod 2 = 0 then 2 (*when n is even the return value is 2 and no other value can be got*)
       else (*when n is odd, make a sub function with 2 ineger arguments with let, in*)
	  let rec temp n divisor =
		if divisor > (n / 2) then n (*when divisor > (n / 2), n is not divided by any divisor, so has to return n*)
		else
		    match (n mod divisor) with
			0 -> divisor (*when n is divided by divisor, return divisor*)
			|_ -> temp n (divisor + 1) in temp n 3;; (*when n is not divided by divisor, increase divisor by 1 and call the function "temp" recursively til divisor becomes larger than n / 2*)
