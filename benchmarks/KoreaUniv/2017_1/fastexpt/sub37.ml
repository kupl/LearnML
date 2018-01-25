(* problem 1*)

let rec fastexpt : int -> int -> int
= fun b n ->  
   if b < 0 then raise (Failure "b has to be positive")(*exclude the case when b is negative*)
   else    
     match n with      
	0 -> 1 (*when n = 0, the return value has to be always 1 no matter what value b is *)      
	|_ -> (*when n is a value other than 0, there are 2 cases; n is even or  odd *)        
	   if n mod 2 = 0 then let square x = x * x in square (fastexpt b (n / 2))
	   (*when n is even, we need to make a sub function "square" which squares the argument, in this case, the  value of fastexpt b (n / 2), by using the let, in.*)
	   else b * fastexpt b (n - 1);;(*when n is odd, we just call function "fastexpt" recursively until n becomes 0*)