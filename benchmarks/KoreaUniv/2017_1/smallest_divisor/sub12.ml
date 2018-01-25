(* problem 2 *)
let smallest_divisor : int -> int = fun n ->
let rec loop i = if (n mod 2)=0 then 2
else if (n mod i)=0 then i
else loop (i+1) in loop 2