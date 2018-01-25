let rec fib n = if n==0 then 0 else if n==1 then 1 else fib(n-2)+fib(n-1);;

let rec pascal(a,b) = if a==b then 1 else if b==0 then 1 else pascal(a-1,b-1)+pascal(a-1,b);;

let rec div (m,n) = if (m mod n <>0)&&n>1 then div (m,n-1) else if n==1 then true else false;;
let rec prime a = if a==0 then false else if a==1 then false else div (a,a-1);;

let rec sigma f a b = if a==b then f a else sigma f (a+1) b + f a;;
