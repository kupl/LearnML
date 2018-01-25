(*problem5*)
let rec dfact : int->int = fun n->
  if n mod 2 = 0 then
    if n=2 then
      n
    else
      n*(dfact (n-2))
  else
    if n=1 then
      n
    else
     n*(dfact (n-2));;