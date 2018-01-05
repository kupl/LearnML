let compose f g x= f (g x)
let identity x=x

let rec iter (n,f)=
  if n<=0 then identity
  else if n=1 then f
  else compose f (iter (n-1,f))

