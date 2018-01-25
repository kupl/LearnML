(* problem 2*)
let rec smallest_divisor : int -> int
= fun n -> 
 match n with 1 -> raise (Failure "Please write a number greater than 1")
 |_ -> let rec aux n i = if i  > (n/2) then n else
 if (n mod i = 0) then i else aux n (i+1) in aux n 2
