let rec sigma (x : int * int * (int -> int)) : int =
let (a, b, f) = x in
if (a > b) then 0
else sigma(a+1, b, f) + f(a)
