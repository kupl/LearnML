(* problem 5*)


  let rec dfact n = 
  if n = 1 then 1
  else if n = 0 then 1
  else n * (dfact n-2);; 
