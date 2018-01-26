let rec div (m,n) = if (m mod n <>0)&&n>1 then div (m,n-1) else if n==1 then true else false;;
let rec prime a = if a==0 then false else if a==1 then false else div (a,a-1);;
