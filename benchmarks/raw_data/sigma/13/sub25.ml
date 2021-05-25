let rec sigma (a,b,x) =
   if b = a then x b
   else if a>b then 0
   else  (x a) + sigma (a+1,b,x)


