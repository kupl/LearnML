exception BAD

let rec sigma (a, b, f) = 
if a < 0 || b < 0 || a > b then raise BAD
else if a = b then f a
else ( f a ) + (sigma((a+1), b, f))

