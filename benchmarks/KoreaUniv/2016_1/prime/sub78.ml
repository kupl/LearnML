let rec prime a n =
if n <= 1 then false else
if n mod a <> 0 then prime (a+1) n else
if n<>a && n mod a = 0 then false
else true;;
