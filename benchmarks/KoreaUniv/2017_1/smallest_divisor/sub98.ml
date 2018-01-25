(* problem 2*)
let smallest_divisor : int -> int
= fun n ->
for i=n to i=2 do
if n mod i = 0 then 
let divisor=i
done;;