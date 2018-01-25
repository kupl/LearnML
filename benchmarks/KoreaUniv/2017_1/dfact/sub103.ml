(* 5 *)
let rec dfact n : int
= if n mod 2 =0
  then (
   if n = 2 then 2
   else n*(dfact (n-2))
  )
  else (
   if n = 1 then 1
   else n*(dfact(n-2))
  );;