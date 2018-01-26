let rec isprim (n, cont) =
if cont=1 then true
else if n mod cont = 0 then false
else isprim (n, (cont-1))

let rec prime n =
if n<2 then false
else if n=2 then true
else if n=3 then true
else if n mod 2 = 0 then false
else if n mod 3 = 0 then false
else isprim (n, int_of_float (sqrt (float_of_int n)))
