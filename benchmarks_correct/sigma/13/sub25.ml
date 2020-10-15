let rec sigma x a b =
   if b = a then x b
   else if a>b then 0
   else  (x a) + sigma x (a+1) b


