let rec check_p di cnt n = (*divide n by 2 to n/2*)
  if di > n/2 then cnt
  else if n mod di = 0 then (check_p (di+1) (cnt+1) n)
  else check_p (di+1) cnt n;;

let prime : int -> bool
= fun n ->
if n < 2 then false
else if (check_p 2 0 n) > 0 then false
else true;;