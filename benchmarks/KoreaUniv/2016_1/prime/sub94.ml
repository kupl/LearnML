# let rec prcheck (n, a) =
if n = a then true
else if (n mod a) = 0 then false
else prcheck(n, a+1);

# let rec prime n =
if n = 1 || n = 2 then true
else prcheck(n, 2);;
