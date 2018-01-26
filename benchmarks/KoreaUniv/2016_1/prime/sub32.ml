let rec prime : int ->  bool
=fun n ->
if n=1 then false
else modd n 2
and modd n k =
if n = k then true
else if n mod k = 0 then false
else modd n (k+1)
