let rec sigma func a b =
if a>b then func b
else if a<b then (func b) + (sigma func a (b-1))

