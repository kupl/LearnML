(*p2*)

let smallest_divisor: int->int
= fun n-> let rec f i =
if (n<i*i) then n
else if ((n mod i)=0) then i
else f (i+1)
in f 2;;