exception Error of string

let rec sigma f a b =
if a>b then
raise (Error "Invalid interval")
else
f a + sigma f (a+1) b;;