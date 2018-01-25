let rec fib
= fun n -> match n with
| 0 -> 0
| 1 -> 1
| n -> fib(n-2) + fib(n-1)





 let rec pascal (n1, n2)
 = if(n1==n2 || n2==0) then 1
   else pascal(n1-1, n2-1) + pascal(n1-1, n2)

