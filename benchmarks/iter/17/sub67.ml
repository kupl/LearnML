let rec iter(n,f) = 
if n==0 then fun x -> x
else if n==1 then f
else fun x-> f (iter(n-1,f) x)

