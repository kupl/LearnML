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

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int) 
= fun (n, f) ->
   if n < 0 then raise (Failure "n has to be positive") (*exclude the case when n is negative*)
   else
       match n with (*there are 2 cases; n is 0 or not*)
       0 -> fun x -> x (*when n is 0, return the identity function*)
       |_ -> (*when is a value other than 0, need to make a sub function "compose_func" for composing functions*)
	  let compose_func f g x = f(g x) in compose_func f (iter ((n - 1), f));;
	  (*The function "compose_func" is mapped because the fucntion "iter" is a function that composes itself. And we need to call the function "iter" recursively til n becomes 0*)

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
   if a < 0 then raise (Failure "a cannot be negative") (*have to exclude the case when the starting point a is negative*)
   else
      if a > b then raise (Failure "b has to be equal to or be larger than a") (*have to exclude the case when the end point b is smaller than the starting point a*)
      else
         if a = b then f b (*when a = b, just return the value of function f of applying the argument b. It is expressed as f b, but f a will be fine as well*)
	 else (f b) * (product f a (b - 1));; (*when b > a, call the function "product" recursively while timing f b, until b becomes eqauls to a*)

(* problem 5*)

let rec dfact : int -> int
= fun n ->
   if n < 0 then raise (Failure "n has to be non-negative") (*exlcude the case when n is negative*)
   else
      match n with
	0 -> 1
	|1 -> 1 (*by the mathematical definition, 0!! or 1!! is 1*)
	|_ -> n * dfact(n - 2);; (*when n is larger than 1, call the function "dfact" recursively, following the rule of getting the value of double factorial, til n becomes 0 or 1*)

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
   match l with
     [] -> [] (*when l is empty, just return an empty list*)
     |hd::tl -> (*when l is not empty, call the function "drop" recursively until n becomees 0*)
	if n = 0 then l else drop tl (n - 1);;

(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
   match lst with
     [] -> ([], []) (*when lst is empty, just return a tuple with 2 empty lists as elements*)
     |(hd1, hd2)::tl ->                      
     		     let (tl1, tl2) = unzip tl in (hd1::tl1, hd2::tl2);; 
	(*When lst is not empty, it means it has a head and a tail. And the head is a tuple in this case, so expressed like (hd1, hd2). And we have to return a tuple with 2 lists as elements, so it is expressed as (tl1, tl2), and just call the function "unzip" recursively until the tl becomes an empty list.*)

(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> (*TODO*)