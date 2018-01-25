(* problem 5*)

let dfact : int -> int
= fun n ->
	if n =1 then 1
	else if n =2 then 2
	else if (n mod 2)=1 then n*(dfact (n-2))
	else n*(dfact (n-2));;