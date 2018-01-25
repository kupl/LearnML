let rec fastexpt b n =
if n = 0 then 1
else if n mod 2 =0 then ( fastexpt (b*b) (n/2))
else b*(fastexpt b (n-1))

let smallest_divisor n =
   let rec sd n i = if i>(n/2) then n
   else if (n mod i = 0) then i
   else sd n (i+1) in sd n 2
