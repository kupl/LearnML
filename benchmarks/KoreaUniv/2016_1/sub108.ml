let rec fib n=
if n<3 then
1
else
fib(n-1) + fib(n-2)


let rec pascal (x,y)=
if x=0 || x=1 || x=y || y=0 then 1
else
    pascal(x-1, y-1) + pascal(x-1, y) 


let rec prime n =
   let rec loop i =
   if n mod i = 0 && n>2 then false
   else if n = i+1 then true
   else if n = 2 then true
   else loop (i+1) 
    in loop 2


let rec sigma f a b=
  if a > b then 0
  else (f a) + sigma f (a+1) b

