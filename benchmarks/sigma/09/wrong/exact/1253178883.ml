exception Error of string

let rec sigma (a,b,f)=
if a>b then
raise (Error "Invalid interval")
else
f a + sigma ((a+1),b,f);;