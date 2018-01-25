let square x = x*x;;

let rec fastexpt b n=
if(n=0) 
  then 1
  else
  if(n mod 2=0)
  then square(fastexpt b (n/2))
  else b*(fastexpt b (n-1));;