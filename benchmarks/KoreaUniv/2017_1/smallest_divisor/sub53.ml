let smallest_divisor n =
   let rec sd n i = if i>(n/2) then n
   else if (n mod i = 0) then i
   else sd n (i+1) in sd n 2
