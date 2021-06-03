let rec iter((n:int), (f:'a->'a)) x=
if (n=0) then x else
if (n<0) then (raise Exit) else
f(iter(n-1,f) x);;
