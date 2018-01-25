(* problem 1*)


 let rec fastexpt b n = 
  if n = 0 then 1
  else if n / 2 =0 then (fastexpt b n / 2) * (fastexpt b n / 2)
  else b * (fastexpt b n-1);;
