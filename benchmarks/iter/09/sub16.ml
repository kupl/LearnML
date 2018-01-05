exception Error of string

let rec iter (n, f) x=
if n<0 then
raise (Error "n should not be negative")
else if n=0 then
x
else
f (iter ((n-1), f) x)